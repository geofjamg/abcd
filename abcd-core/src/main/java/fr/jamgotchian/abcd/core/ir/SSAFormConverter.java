/*
 * Copyright (C) 2011 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.jamgotchian.abcd.core.ir;

import fr.jamgotchian.abcd.graph.DominatorInfo;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.util.Counter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Convert IR to pruned SSA form.
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SSAFormConverter {

    private static final Logger LOGGER = LoggerFactory.getLogger(SSAFormConverter.class);

    private final ControlFlowGraph graph;

    private final IRInstFactory instFactory;

    private final VariableFactory varFactory;

    private Multimap<BasicBlock, Integer> liveVariables;

    private Multimap<Integer, BasicBlock> defBlocks;

    private Set<Integer> globals;

    public SSAFormConverter(ControlFlowGraph graph, IRInstFactory instFactory,
                            VariableFactory varFactory) {
        this.graph = graph;
        this.instFactory = instFactory;
        this.varFactory = varFactory;
    }

    private static boolean containsDef(BasicBlock block, int defIndex) {
        for (IRInst inst : block.getInstructions()) {
            if (inst instanceof DefInst) {
                Variable def = ((DefInst) inst).getResult();
                if (def.getIndex() == defIndex) {
                    return true;
                }
            }
        }
        return false;
    }

    private void insertPhiFunctions() {
        DominatorInfo<BasicBlock, Edge> dominatorInfo = graph.getDominatorInfo();

        Multimap<Integer, BasicBlock> phi = HashMultimap.create();
        for (int defIndex : defBlocks.keySet()) {
            if (globals.contains(defIndex)) {
                Set<BasicBlock> w = new HashSet<>(defBlocks.get(defIndex));
                while (w.size() > 0) {
                    BasicBlock n = w.iterator().next();
                    w.remove(n);
                    Set<Edge> frontier = dominatorInfo.getDominanceFrontierOf(n);
                    for (Edge e : frontier) {
                        BasicBlock y = graph.getEdgeTarget(e);
                        if (!phi.get(defIndex).contains(y)) {
                            boolean contains = containsDef(y, defIndex);
                            List<Variable> args = new ArrayList<>();
                            for (int i = 0; i < graph.getPredecessorCountOf(y); i++) {
                                args.add(varFactory.create(defIndex, y, -1));
                            }
                            // is definition alive in basic block y ?
                            if (liveVariables.get(y).contains(defIndex)) {
                                y.getInstructions()
                                 .insertAt(0, instFactory.newPhi(varFactory.create(defIndex, y, -1), args));
                                LOGGER.trace("  Add Phi function to {} for var {}", y, defIndex);
                            }
                            phi.put(defIndex, y);
                            if (!contains) {
                                w.add(y);
                            }
                        }
                    }
                }
            }
        }
    }

    private void renameVariables() {
        for (int defIndex : defBlocks.keySet()) {
            if (globals.contains(defIndex)) {
                Counter versionCount = new Counter(0);
                Deque<Integer> versionStack = new ArrayDeque<>();
                versionStack.push(0);
                renameVariables(defIndex, graph.getEntryBlock(), versionStack, versionCount);
            }
        }
    }

    private void renameVariables(int varIndex, BasicBlock n, Deque<Integer> versionStack,
                                 Counter versionCount) {
        LOGGER.trace("  Rename variables {} of {}", varIndex, n);

        // generated version in current block
        Set<Integer> generatedVersion = new HashSet<>();

        for (IRInst inst : n.getInstructions()) {
            // for each use of the variable rename with the current version
            // (version on top of version stack)
            if (!(inst instanceof PhiInst)) {
                for (Variable use : inst.getUses()) {
                    if (use.getIndex() == varIndex) {
                        int version = versionStack.peek();
                        use.setVersion(version);
                    }
                }
            }
            // for each new definition of the variable create and push a new
            // version on version stack
            if (inst instanceof DefInst) {
                Variable def = ((DefInst) inst).getResult();
                if (def.getIndex() == varIndex) {
                    versionCount.increment();
                    int nextVersion = versionCount.getCount();
                    versionStack.push(nextVersion);
                    def.setVersion(nextVersion);
                    generatedVersion.add(nextVersion);
                }
            }
        }

        // fill in phi function parameters of successor blocks
        for (BasicBlock y : graph.getSuccessorsOf(n)) {
            for (IRInst inst : y.getInstructions()) {
                if (inst instanceof PhiInst) {
                    int j = new ArrayList<>(graph.getPredecessorsOf(y)).indexOf(n);
                    int version = versionStack.peek();
                    Variable phi = ((PhiInst) inst).getArgs().get(j);
                    if (phi.getIndex() == varIndex) {
                        phi.setVersion(version);
                    }
                }
            }
        }

        // recurse on current block children in the dominator tree
        DominatorInfo<BasicBlock, Edge> dominatorInfo = graph.getDominatorInfo();
        for (BasicBlock x : dominatorInfo.getDominatorsTree().getChildren(n)) {
            renameVariables(varIndex, x, versionStack, versionCount);
        }

        // remove from the version stack, versions generated in current block
        for (Integer version : generatedVersion) {
            versionStack.remove(version);
        }
    }

    private static void addInst(BasicBlock bb, IRInst inst) {
        IRInstSeq insts = bb.getInstructions();
        if (insts.isEmpty()) {
            insts.add(inst);
        } else {
            IRInst lastInst = insts.get(insts.size()-1);
            if (lastInst instanceof JumpIfInst) {
                insts.insertAt(insts.size()-1, inst);
            } else {
                insts.add(inst);
            }
        }
    }

    private void removePhiFunctions() {
        for (BasicBlock bb : graph.getBasicBlocks()) {
            List<BasicBlock> predecessors
                    = new ArrayList<>(graph.getPredecessorsOf(bb));
            IRInstSeq insts = bb.getInstructions();
            for (Iterator<IRInst> it = insts.iterator(); it.hasNext();) {
                IRInst inst = it.next();
                if (inst instanceof PhiInst) {
                    PhiInst phiInst = (PhiInst) inst;
                    for (int i = 0; i < phiInst.getArgs().size(); i++) {
                        Variable result = phiInst.getResult();
                        Variable v = phiInst.getArgs().get(i);
                        BasicBlock p = predecessors.get(i);
                        addInst(p, instFactory.newAssignVar(new Variable(result),
                                                            v /* no need to clone */));
                    }
                    it.remove();
                }
            }
        }
    }

    public void convert() {
        // live variables analysis
        liveVariables = HashMultimap.create();
        for (Map.Entry<BasicBlock, Set<Variable>> entry :
                new LiveVariablesAnalysis(graph).analyse().entrySet()) {
            BasicBlock block = entry.getKey();
            for (Variable var : entry.getValue()) {
                if (var.getVersion() != VariableID.UNDEFINED_VERSION) {
                    throw new ABCDException("var.getVersion() != VariableID.UNDEFINED_VERSION");
                }
                liveVariables.put(block, var.getIndex());
            }
        }

        LOGGER.debug("Convert to SSA form");

        // fill map containing all definitions and its basic block
        defBlocks = HashMultimap.create();
        for (BasicBlock block : graph.getBasicBlocks()) {
            for (IRInst inst : block.getInstructions()) {
                if (inst instanceof DefInst) {
                    Variable def = ((DefInst) inst).getResult();
                    if (!def.isTemporary()) {
                        defBlocks.put(def.getIndex(), block);
                    }
                }
            }
        }

        // local liveness : ie, used before defined in any basic block
        globals = new HashSet<>();
        for (BasicBlock bb : graph.getBasicBlocks()) {
            Set<Integer> defined = new HashSet<>();
            for (IRInst inst : bb.getInstructions()) {
                for (Variable use : inst.getUses()) {
                    if (!use.isTemporary() && !defined.contains(use.getIndex())) {
                        globals.add(use.getIndex());
                    }
                }
                if (inst instanceof DefInst) {
                    Variable def = ((DefInst) inst).getResult();
                    if (!def.isTemporary()) {
                        defined.add(def.getIndex());
                    }
                }
            }
        }
        LOGGER.trace("Local liveness {}", globals);

        insertPhiFunctions();

        renameVariables();

        removePhiFunctions();
    }
}
