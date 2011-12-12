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

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.common.ABCDWriter;
import fr.jamgotchian.abcd.core.graph.DominatorInfo;
import fr.jamgotchian.abcd.core.graph.PostDominatorInfo;
import fr.jamgotchian.abcd.core.graph.Tree;
import static fr.jamgotchian.abcd.core.ir.BasicBlockPropertyName.*;
import fr.jamgotchian.abcd.core.type.ClassNameManager;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IntermediateRepresentationBuilder {

    private static final Logger LOGGER
            = Logger.getLogger(IntermediateRepresentationBuilder.class.getName());

    private final ControlFlowGraphBuilder cfgBuilder;

    private final InstructionBuilder instBuilder;

    private final ClassNameManager classNameManager;

    private final TemporaryVariableFactory tmpVarFactory;

    private final IRInstFactory instFactory;

    private final VariableNameProviderFactory nameProviderFactory;

    private final JavaType thisType;

    private final boolean staticMethod;

    private final JavaType methodReturnType;

    private final List<Variable> methodArgs;

    private ControlFlowGraph cfg;

    public IntermediateRepresentationBuilder(ControlFlowGraphBuilder cfgBuilder,
                                             InstructionBuilder instBuilder,
                                             ClassNameManager classNameManager,
                                             TemporaryVariableFactory tmpVarFactory,
                                             IRInstFactory instFactory,
                                             VariableNameProviderFactory nameProviderFactory,
                                             JavaType thisType,
                                             boolean staticMethod,
                                             JavaType methodReturnType,
                                             List<Variable> methodArgs) {
        this.cfgBuilder = cfgBuilder;
        this.instBuilder = instBuilder;
        this.classNameManager = classNameManager;
        this.tmpVarFactory = tmpVarFactory;
        this.instFactory = instFactory;
        this.nameProviderFactory = nameProviderFactory;
        this.thisType = thisType;
        this.staticMethod = staticMethod;
        this.methodReturnType = methodReturnType;
        this.methodArgs = methodArgs;
    }

    /**
     * Replace choice instructions by conditional instructions (ternary operator)
     */
    public void resolveChoiceInst() {
        ConsoleUtil.logTitledSeparator(LOGGER, Level.FINE,
                "Resolve choice instructions of {0}", '=', cfg.getName());

        for (BasicBlock joinBlock : cfg.getDFST()) {
            IRInstSeq joinInsts = joinBlock.getInstructions();

            for (int i = 0; i < joinInsts.size(); i++) {
                IRInst inst = joinInsts.get(i);
                if (!(inst instanceof ChoiceInst)) {
                    continue;
                }
                ChoiceInst choiceInst = (ChoiceInst) inst;

                List<IRInst> condInsts = new ArrayList<IRInst>();

                boolean change = true;
                while (change) {
                    change = false;

                    Multimap<BasicBlock, Variable> forkBlocks
                            = HashMultimap.create();
                    for (Variable var : choiceInst.getChoices()) {
                        BasicBlock block = var.getBasicBlock();
                        DominatorInfo<BasicBlock, Edge> domInfo = cfg.getDominatorInfo();
                        BasicBlock forkBlock = domInfo.getDominatorsTree().getParent(block);
                        forkBlocks.put(forkBlock, var);
                    }

                    for (Map.Entry<BasicBlock, Collection<Variable>> entry
                            : forkBlocks.asMap().entrySet()) {
                        BasicBlock forkBlock = entry.getKey();
                        Collection<Variable> vars = entry.getValue();
                        if (forkBlock.getType() == BasicBlockType.JUMP_IF
                                && vars.size() == 2) {
                            Iterator<Variable> it = vars.iterator();
                            Variable var1 = it.next();
                            Variable var2 = it.next();

                            BasicBlock block1 = var1.getBasicBlock();
                            BasicBlock block2 = var2.getBasicBlock();
                            PostDominatorInfo<BasicBlock, Edge> postDomInfo = cfg.getPostDominatorInfo();
                            Edge forkEdge1 = postDomInfo.getPostDominanceFrontierOf(block1).iterator().next();
                            Edge forkEdge2 = postDomInfo.getPostDominanceFrontierOf(block2).iterator().next();
                            Variable thenVar = null;
                            Variable elseVar = null;
                            if (Boolean.TRUE.equals(forkEdge1.getValue())
                                    && Boolean.FALSE.equals(forkEdge2.getValue())) {
                                thenVar = var1;
                                elseVar = var2;
                            } else if (Boolean.FALSE.equals(forkEdge1.getValue())
                                    && Boolean.TRUE.equals(forkEdge2.getValue())) {
                                thenVar = var2;
                                elseVar = var1;
                            }
                            if (thenVar != null && elseVar != null) {
                                JumpIfInst jumpIfInst = (JumpIfInst) forkBlock.getInstructions().getLast();
                                choiceInst.getChoices().remove(thenVar);
                                choiceInst.getChoices().remove(elseVar);
                                Variable condVar = jumpIfInst.getCond().clone();
                                if (choiceInst.getChoices().isEmpty()) {
                                    Variable resultVar = choiceInst.getResult();
                                    ConditionalInst condInst
                                            = instFactory.newConditional(resultVar, condVar, thenVar, elseVar);
                                    LOGGER.log(Level.FINER, "Replace inst at {0} of {1} : {2}",
                                            new Object[]{i, joinBlock, IRInstWriter.toText(condInst)});
                                    condInsts.add(condInst);
                                } else {
                                    Variable resultVar = tmpVarFactory.create(forkBlock);
                                    ConditionalInst condInst
                                            = instFactory.newConditional(resultVar, condVar, thenVar, elseVar);
                                    LOGGER.log(Level.FINER, "Insert inst at {0} of {1} : {2}",
                                            new Object[]{i, joinBlock, IRInstWriter.toText(condInst)});
                                    condInsts.add(condInst);
                                    choiceInst.getChoices().add(resultVar);
                                }

                                change = true;
                            } else {
                                throw new ABCDException("Conditional instruction building error");
                            }
                        } else if (forkBlock.getType() == BasicBlockType.SWITCH
                                && vars.size() > 2) {
                            throw new ABCDException("TODO");
                        }
                    }
                }

                // replace the choice inst by the list of conditional instructions
                if (condInsts.size() > 0) {
                    joinInsts.remove(i);
                    joinInsts.addAll(i, condInsts);
                } else {
                    throw new ABCDException("Fail to resolve choice instruction "
                            + IRInstWriter.toText(inst));
                }
            }
        }
    }

    private void addFakeEdges() {
        for (BasicBlock bb : cfg.getBasicBlocks()) {
            if (bb.equals(cfg.getEntryBlock()) || bb.equals(cfg.getExitBlock())) {
                continue;
            }
            IRInstSeq insts = bb.getInstructions();
            if (insts == null) {
                throw new ABCDException("insts == null");
            }
            if (cfg.getSuccessorCountOf(bb) == 0
                    && insts.getLast() instanceof ThrowInst) {
                Edge fakeEdge = cfg.addEdge(bb, cfg.getExitBlock());
                fakeEdge.addAttribute(EdgeAttribute.FAKE_EDGE);
                LOGGER.log(Level.FINEST, "Add fake edge {0}", cfg.toString(fakeEdge));
            }
        }
        for (NaturalLoop loop : cfg.getNaturalLoops().values()) {
            if (loop.getExits().isEmpty()) { // infinite loop
                Edge fakeEdge = cfg.addEdge(loop.getHead(), cfg.getExitBlock());
                fakeEdge.addAttribute(EdgeAttribute.FAKE_EDGE);
                LOGGER.log(Level.FINEST, "Add fake edge {0}", cfg.toString(fakeEdge));
            }
        }
    }

    private void assignNameToVariables() {
        VariableNameProvider nameProvider = nameProviderFactory.create(cfg);

        Set<Variable> variables = new HashSet<Variable>();

        for (Variable arg : methodArgs) {
            arg.setName(nameProvider.getName(arg, staticMethod));
        }
        for (BasicBlock block : cfg.getBasicBlocks()) {
            for (IRInst inst : block.getInstructions()) {
                if (inst instanceof DefInst) {
                    Variable def = ((DefInst) inst).getResult();
                    if (!def.isTemporary()) {
                        variables.add(def);
                        def.setName(nameProvider.getName(def, staticMethod));
                    }
                }
                for (Variable use : inst.getUses()) {
                    if (!use.isTemporary()) {
                        variables.add(use);
                        use.setName(nameProvider.getName(use, staticMethod));
                    }
                }
            }
        }

        List<String> variableColumn = new ArrayList<String>(1);
        List<String> positionColumn = new ArrayList<String>(1);
        List<String> nameColumn = new ArrayList<String>(1);
        variableColumn.add("Variable");
        positionColumn.add("Position");
        nameColumn.add("Name");
        for (Variable v : variables) {
            variableColumn.add(v.getID().toString());
            positionColumn.add(Integer.toString(v.getPosition()));
            nameColumn.add(v.getName() != null ? v.getName() : "<undefined>");
        }
        LOGGER.log(Level.FINEST, "Variable names :\n{0}",
                ConsoleUtil.printTable(variableColumn, positionColumn, nameColumn));
    }

    private void addVariableDeclarations() {
        Multimap<VariableID, BasicBlock> defBlocks = HashMultimap.create();
        for (BasicBlock block : cfg.getBasicBlocks()) {
            for (IRInst inst : block.getInstructions()) {
                if (!inst.isIgnored() && inst instanceof DefInst) {
                    Variable def = ((DefInst) inst).getResult();
                    if (!def.isTemporary()) {
                        defBlocks.put(def.getID(), block);
                    }
                }
            }
        }
        Tree<BasicBlock, Edge> domTree = cfg.getDominatorInfo().getDominatorsTree();
        for (Map.Entry<VariableID, Collection<BasicBlock>> entry : defBlocks.asMap().entrySet()) {
            VariableID ID = entry.getKey();
            BasicBlock commonAncestor = domTree.getFirstCommonAncestor(entry.getValue());
            assert commonAncestor != null;
            if (commonAncestor.hasProperty(VARIABLE_DECLARATION)) {
                ((Set<VariableID>) commonAncestor.getProperty(VARIABLE_DECLARATION)).add(ID);
            } else {
                commonAncestor.putProperty(VARIABLE_DECLARATION, Sets.newHashSet(ID));
            }
            LOGGER.log(Level.FINER, "Declar variable {0} at {1} ",
                    new Object[] {ID, commonAncestor});
        }
    }

    public ControlFlowGraph build(ABCDWriter writer) {
        // build control flow graph from bytecode
        cfg = cfgBuilder.build();

        cfg.removeUnreachableBlocks();
        cfg.updateDominatorInfo();
        cfg.updateLoopInfo();

        writer.writeRawCFG(cfg, cfgBuilder.getGraphizRenderer());

        try {
            // merge natural loops
            if (cfg.mergeNaturalLoops()) {
                cfg.updateDominatorInfo();
                cfg.updateLoopInfo();
            }

            // build basic blocks instructions
            instBuilder.build(cfg);

            if (cfg.removeUnnecessaryBlock()) {
                cfg.updateDominatorInfo();
                cfg.updateLoopInfo();
            }

            // add fake edges to be able to compute post dominance in case of infinite
            // loops et throw instructions
            addFakeEdges();

            cfg.updatePostDominatorInfo();

            // collapse shortcut operators (&&, ||)
            ShortcutOperatorsCollapser collapser
                    = new ShortcutOperatorsCollapser(cfg, tmpVarFactory, instFactory);
            if (collapser.collapse()) {
                cfg.updateDominatorInfo();
                cfg.updatePostDominatorInfo();
                cfg.updateLoopInfo();
            }

            // must be done after collapsing shortcut operators because of conditional
            // instruction with shortcut operators in the condition
            resolveChoiceInst();

            // need to remove critical edges to convert to SSA
            if (cfg.removeCriticalEdges()) {
                cfg.updateDominatorInfo();
                cfg.updatePostDominatorInfo();
                cfg.updateLoopInfo();
            }

            // convert to SSA form
//            new SSAFormConverter(cfg, instFactory).convert();

            // to remove empty basic blocks added tu remove critical edges
            if (cfg.removeUnnecessaryBlock()) {
                cfg.updateDominatorInfo();
                cfg.updatePostDominatorInfo();
                cfg.updateLoopInfo();
            }

            // add variable declarations
            addVariableDeclarations();

            // analyse local variables types
            new LocalVariableTypeAnalyser(cfg, thisType, methodReturnType,
                                          methodArgs, classNameManager)
                    .analyse();

            // assign a name to each variable
            assignNameToVariables();

            writer.writeCFG(cfg, false);
        } finally {
            writer.writeCFG(cfg, true);
        }

        return cfg;
    }
}
