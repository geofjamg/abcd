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
package fr.jamgotchian.abcd.core.analysis;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.DominatorInfo;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.tac.model.PhiFunctionInst;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.tac.util.TACInstWriter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SSAFormConverter {

    private static final Logger logger = Logger.getLogger(SSAFormConverter.class.getName());

    private final ControlFlowGraph graph;

    private Map<BasicBlock, Set<Variable>> liveVariables;

    private Set<Variable> defs;

    private Multimap<Variable, BasicBlock> defSites;

    public SSAFormConverter(ControlFlowGraph graph) {
        this.graph = graph;
    }

    private static boolean containsDef(BasicBlock block, Variable def) {
        for (TACInst inst : ((AnalysisData) block.getData()).getInstructions()) {
            if (def.equals(inst.getDef())) {
                return true;
            }
        }
        return false;
    }

    private void insertPhiFunctions() {
        DominatorInfo<BasicBlock, Edge> dominatorInfo = graph.getDominatorInfo();

        Multimap<Variable, BasicBlock> phi = HashMultimap.create();
        for (Variable def : defs) {
            Set<BasicBlock> w = new HashSet<BasicBlock>(defSites.get(def));
            while (w.size() > 0) {
                BasicBlock n = w.iterator().next();
                w.remove(n);
                Set<Edge> frontier = dominatorInfo.getDominanceFrontierOf(n);
                for (Edge e : frontier) {
                    BasicBlock y = graph.getEdgeTarget(e);
                    if (!phi.get(def).contains(y)) {
                        boolean contains = containsDef(y, def);
                        List<Variable> args = new ArrayList<Variable>();
                        for (int i = 0; i < graph.getPredecessorCountOf(y); i++) {
                            args.add(def.clone());
                        }
                        // is definition alive in basic block y ?
                        if (liveVariables.get(y).contains(def)) {
                            ((AnalysisData) y.getData()).getInstructions()
                                    .add(0, new PhiFunctionInst(def.clone(), args));
                            logger.log(Level.FINEST, "Add Phi function to {0} for def {1}",
                                    new Object[] {y, TACInstWriter.toText(def)});
                        }
                        phi.put(def, y);
                        if (!contains) {
                            w.add(y);
                        }
                    }
                }
            }
        }
    }

    private void renameVariables() {
        for (Variable v : defs) {
            AtomicInteger versionCount = new AtomicInteger(0); // used as a mutable integer
            Deque<Integer> versionStack = new ArrayDeque<Integer>();
            versionStack.push(0);
            renameVariables(v, graph.getEntryBlock(), versionStack, versionCount);
        }
    }

    private void renameVariables(Variable v, BasicBlock n, Deque<Integer> versionStack,
                                 AtomicInteger versionCount) {
        logger.log(Level.FINEST, "  Rename variables of {0}", n);

        for (TACInst inst : ((AnalysisData) n.getData()).getInstructions()) {
            if (!(inst instanceof PhiFunctionInst)) {
                for (Variable use : inst.getUses()) {
                    if (use.equals(v)) {
                        int version = versionStack.peek();
                        use.setVersion(version);
                    }
                }
            }
                Variable def = inst.getDef();
                if (def != null && def.equals(v)) {
                    int nextVersion = versionCount.incrementAndGet();
                    versionStack.push(nextVersion);
                    def.setVersion(nextVersion);
                }
        }

        for (BasicBlock y : graph.getSuccessorsOf(n)) {
            for (TACInst inst : ((AnalysisData) y.getData()).getInstructions()) {
                if (inst instanceof PhiFunctionInst) {
                    int j = new ArrayList<BasicBlock>(graph.getPredecessorsOf(y)).indexOf(n);
                    int version = versionStack.peek();
                    Variable phi = ((PhiFunctionInst) inst).getArgs().get(j);
                    if (phi.equals(v)) {
                        phi.setVersion(version);
                    }
                }
            }
        }

        DominatorInfo<BasicBlock, Edge> dominatorInfo = graph.getDominatorInfo();
        for (BasicBlock x : dominatorInfo.getDominatorsTree().getChildren(n)) {
            renameVariables(v, x, versionStack, versionCount);
        }

        for (TACInst inst : ((AnalysisData) n.getData()).getInstructions()) {
            if (!(inst instanceof PhiFunctionInst)) {
                Variable def = inst.getDef();
                if (def != null && def.equals(v)) {
                    versionStack.pop();
                }
            }
        }
    }

    public void convert() {
        liveVariables = new LiveVariablesAnalysis(graph).analyse();

        defs = new HashSet<Variable>();
        defSites = HashMultimap.create();
        for (BasicBlock block : graph.getBasicBlocks()) {
            for (TACInst inst : ((AnalysisData) block.getData()).getInstructions()) {
                Variable def = inst.getDef();
                if (def != null) {
                    defs.add(def);
                    defSites.put(def, block);
                }
            }
        }

        insertPhiFunctions();

        renameVariables();
    }
}
