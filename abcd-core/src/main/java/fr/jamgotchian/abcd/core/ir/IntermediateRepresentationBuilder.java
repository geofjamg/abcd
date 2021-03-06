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
import fr.jamgotchian.abcd.core.common.Configuration;
import fr.jamgotchian.abcd.core.common.DecompilationObserver;
import fr.jamgotchian.abcd.graph.DominatorInfo;
import fr.jamgotchian.abcd.graph.PostDominatorInfo;
import fr.jamgotchian.abcd.graph.Tree;
import static fr.jamgotchian.abcd.core.ir.BasicBlockPropertyName.*;
import fr.jamgotchian.abcd.core.type.ClassNameManager;
import fr.jamgotchian.abcd.core.util.console.ConsoleUtil;
import fr.jamgotchian.abcd.core.util.console.TablePrinter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IntermediateRepresentationBuilder {

    private static final Logger LOGGER
            = LoggerFactory.getLogger(IntermediateRepresentationBuilder.class);

    private final ControlFlowGraph cfg;

    private final LocalVariableTable localVarTable;

    private final InstructionBuilder instBuilder;

    private final ClassNameManager classNameManager;

    private final VariableFactory varFactory;

    private final IRInstFactory instFactory;

    private final VariableNameProviderFactory nameProviderFactory;

    private final MethodContext methodContext;

    private final DecompilationObserver observer;

    private final Configuration config;

    private final ClassLoader classLoader;

    public IntermediateRepresentationBuilder(ControlFlowGraph cfg,
                                             LocalVariableTable localVarTable,
                                             InstructionBuilder instBuilder,
                                             ClassNameManager classNameManager,
                                             VariableFactory varFactory,
                                             IRInstFactory instFactory,
                                             VariableNameProviderFactory nameProviderFactory,
                                             MethodContext methodContext,
                                             DecompilationObserver observer,
                                             Configuration config,
                                             ClassLoader classLoader) {
        this.cfg = cfg;
        this.localVarTable = localVarTable;
        this.instBuilder = instBuilder;
        this.classNameManager = classNameManager;
        this.varFactory = varFactory;
        this.instFactory = instFactory;
        this.nameProviderFactory = nameProviderFactory;
        this.methodContext = methodContext;
        this.observer = observer;
        this.config = config;
        this.classLoader = classLoader;
    }

    /**
     * Replace choice instructions by conditional instructions (ternary operator)
     */
    public void resolveChoiceInst() {
        LOGGER.debug(ConsoleUtil.formatTitledSeparator("Resolve choice instructions of {}", '='),
                cfg.getName());

        for (BasicBlock joinBlock : cfg.getDFST()) {
            IRInstSeq joinInsts = joinBlock.getInstructions();

            for (int i = 0; i < joinInsts.size(); i++) {
                IRInst inst = joinInsts.get(i);
                if (!(inst instanceof ChoiceInst)) {
                    continue;
                }
                ChoiceInst choiceInst = (ChoiceInst) inst;

                List<IRInst> condInsts = new ArrayList<>();

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
                                Variable condVar = new Variable(jumpIfInst.getCond());
                                if (choiceInst.getChoices().isEmpty()) {
                                    Variable resultVar = choiceInst.getResult();
                                    ConditionalInst condInst
                                            = instFactory.newConditional(resultVar, condVar, thenVar, elseVar);
                                    LOGGER.debug("Replace inst at {} of {} : {}",
                                            new Object[]{i, joinBlock, IRInstWriter.toText(condInst)});
                                    condInsts.add(condInst);
                                } else {
                                    Variable resultVar = varFactory.createTmp(forkBlock);
                                    ConditionalInst condInst
                                            = instFactory.newConditional(resultVar, condVar, thenVar, elseVar);
                                    LOGGER.debug("Insert inst at {} of {} : {}",
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

    private void assignNameToVariables() {
        VariableNameProvider nameProvider = nameProviderFactory.create(localVarTable);

        Set<Variable> variables = new HashSet<>();

        for (Variable arg : methodContext.getArguments()) {
            arg.setName(nameProvider.getName(arg));
        }
        for (BasicBlock block : cfg.getBasicBlocks()) {
            for (IRInst inst : block.getInstructions()) {
                if (inst instanceof DefInst) {
                    Variable def = ((DefInst) inst).getResult();
                    if (!def.isTemporary()) {
                        variables.add(def);
                        def.setName(nameProvider.getName(def));
                    }
                }
                for (Variable use : inst.getUses()) {
                    if (!use.isTemporary()) {
                        variables.add(use);
                        use.setName(nameProvider.getName(use));
                    }
                }
            }
        }

        TablePrinter printer = ConsoleUtil.newTablePrinter("Variable", "Position", "Name");
        for (Variable v : variables) {
            printer.addRow(v.getID().toString(),
                           v.getPosition(),
                           v.getName() != null ? v.getName() : "<undefined>");
        }
        LOGGER.trace("Variable names :\n{}", printer.toString());
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
            LOGGER.debug("Declar variable {} at {} ", ID, commonAncestor);
        }
    }

    public void build() {

        cfg.removeUnreachableBlocks();
        cfg.updateDominatorInfo();
        cfg.updateLoopInfo();

        observer.doneRawCFG(cfg);

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

            cfg.ensureSingleExit();
            cfg.updatePostDominatorInfo();

            // collapse shortcut operators (&&, ||)
//            ShortcutOperatorsCollapser collapser
//                    = new ShortcutOperatorsCollapser(cfg, varFactory, instFactory);
//            if (collapser.collapse()) {
//                cfg.updateDominatorInfo();
//                cfg.updatePostDominatorInfo();
//                cfg.updateLoopInfo();
//            }
//
//            // must be done after collapsing shortcut operators because of conditional
//            // instruction with shortcut operators in the condition
//            resolveChoiceInst();
//
//            if (config.isAnalyseLocalVariableType()) {
//                // need to remove critical edges to convert to SSA
//                if (cfg.removeCriticalEdges()) {
//                    cfg.updateDominatorInfo();
//                    cfg.updatePostDominatorInfo();
//                    cfg.updateLoopInfo();
//                }
//
//                // convert to SSA form
//                new SSAFormConverter(cfg, instFactory, varFactory).convert();
//
//                // to remove empty basic blocks added tu remove critical edges
//                if (cfg.removeUnnecessaryBlock()) {
//                    cfg.updateDominatorInfo();
//                    cfg.updatePostDominatorInfo();
//                    cfg.updateLoopInfo();
//                }
//
//                // analyse local variables types
//                new LocalVariableTypeAnalyser(cfg, methodContext, classNameManager,
//                                              varFactory, classLoader)
//                        .analyse();
//            }
//
//            // add variable declarations
//            addVariableDeclarations();
//
//            // assign a name to each variable
//            assignNameToVariables();

            observer.doneCFG(cfg, false);
        } finally {
            observer.doneCFG(cfg, true);
        }
    }
}
