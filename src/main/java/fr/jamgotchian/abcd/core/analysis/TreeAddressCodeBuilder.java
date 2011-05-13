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
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockType;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.DominatorInfo;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.tac.model.AssignConstInst;
import fr.jamgotchian.abcd.core.tac.model.ChoiceInst;
import fr.jamgotchian.abcd.core.tac.model.ConditionalInst;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
import fr.jamgotchian.abcd.core.tac.model.LocalVariable;
import fr.jamgotchian.abcd.core.tac.model.StringConst;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.tac.util.TACInstWriter;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import java.util.ArrayDeque;
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
public class TreeAddressCodeBuilder {

    private static final Logger logger = Logger.getLogger(TreeAddressCodeBuilder.class.getName());

    private final ControlFlowGraph graph;

    private final ClassNameFactory classNameFactory;

    private TemporaryVariableFactory tmpVarFactory;

    public TreeAddressCodeBuilder(ControlFlowGraph graph, ClassNameFactory classNameFactory) {
        this.graph = graph;
        this.classNameFactory = classNameFactory;
    }

    private void processBlock(BasicBlock block, List<ArrayDeque<LocalVariable>> inputStacks) {

        logger.log(Level.FINER, "------ Process block {0} ------", block);

        AnalysisData data = (AnalysisData) block.getData();

        ArrayDeque<LocalVariable> inputStack = null;
        if (inputStacks.isEmpty()) {
            inputStack = new ArrayDeque<LocalVariable>();
        } else if (inputStacks.size() == 1) {
            inputStack = inputStacks.get(0).clone();
        } else {
            inputStack = mergeStacks(inputStacks, block);
        }

        data.setInputStack2(inputStack.clone());

        ArrayDeque<LocalVariable> outputStack = inputStack.clone();
        Iterator<Edge> itE = graph.getIncomingEdgesOf(block).iterator();
        if (itE.hasNext() && itE.next().isExceptional()) {
            LocalVariable tmpVar = tmpVarFactory.create(block);
            BasicBlock3ACBuilder.addInst(block, new AssignConstInst(tmpVar, new StringConst("EXCEPTION", classNameFactory)));
            outputStack.push(tmpVar);
        }

        if (data.getInputStack2().size() > 0) {
            logger.log(Level.FINEST, ">>> Input stack : {0}", data.getInputStack2());
        }

        BasicBlock3ACBuilder builder = new BasicBlock3ACBuilder(classNameFactory, tmpVarFactory, outputStack);
        block.visit(builder);
        data.setOutputStack2(outputStack);

        if (data.getOutputStack().size() > 0) {
            logger.log(Level.FINEST, "<<< Output stack : {0}", data.getOutputStack2());
        }
    }

    private ArrayDeque<LocalVariable> mergeStacks
            (List<ArrayDeque<LocalVariable>> stacks, BasicBlock block) {
        if (stacks.size() <= 1) {
            throw new ABCDException("stacks.size() <= 1");
        }
        List<Integer> sizes = new ArrayList<Integer>(stacks.size());
        for (int i = 0; i < stacks.size(); i++) {
            sizes.add(stacks.get(i).size());
        }
        for (int i = 0; i < sizes.size() - 1; i++) {
            if (sizes.get(i) != sizes.get(i + 1)) {
                throw new ABCDException("Cannot merge stacks with differents sizes : "
                        + sizes);
            }
        }

        ArrayDeque<LocalVariable> stacksMerge
                = new ArrayDeque<LocalVariable>(stacks.get(0).size());

        List<List<LocalVariable>> toList
                = new ArrayList<List<LocalVariable>>(stacks.size());
        for (int i = 0; i < stacks.size(); i++) {
            toList.add(new ArrayList<LocalVariable>(stacks.get(i)));
        }
        for (int i = 0; i < stacks.get(0).size(); i++) {
            Set<LocalVariable> vars = new HashSet<LocalVariable>(stacks.size());
            for (int j = 0; j < stacks.size(); j++) {
                vars.add(toList.get(j).get(i));
            }
            if (vars.size() == 1) {
                stacksMerge.add(vars.iterator().next());
            } else {
                LocalVariable result = tmpVarFactory.create(block);
                BasicBlock3ACBuilder.addInst(block, new ChoiceInst(result, vars));
                stacksMerge.add(result);
            }
        }

        return stacksMerge;
    }

    private void removeChoiceInst() {
        for (BasicBlock joinBlock : graph.getDFST()) {
            List<TACInst> joinInsts = ((AnalysisData) joinBlock.getData()).getInstructions();

            for (int i = 0; i < joinInsts.size(); i++) {
                TACInst inst = joinInsts.get(i);
                if (!(inst instanceof ChoiceInst)) {
                    continue;
                }
                ChoiceInst choiceInst = (ChoiceInst) inst;

                List<TACInst> replacement = new ArrayList<TACInst>();

                boolean change = true;
                while (change) {
                    change = false;

                    Multimap<BasicBlock, LocalVariable> forkBlocks
                            = HashMultimap.create();
                    for (LocalVariable var : choiceInst.getChoices()) {
                        BasicBlock block = var.getBasicBlock();
                        DominatorInfo<BasicBlock, Edge> dominatorInfo
                                = block.getGraph().getDominatorInfo();
                        BasicBlock forkBlock = dominatorInfo.getDominatorsTree().getParent(block);
                        forkBlocks.put(forkBlock, var);
                    }

                    for (Map.Entry<BasicBlock, Collection<LocalVariable>> entry
                            : forkBlocks.asMap().entrySet()) {
                        BasicBlock forkBlock = entry.getKey();
                        Collection<LocalVariable> vars = entry.getValue();
                        if (forkBlock.getType() == BasicBlockType.JUMP_IF
                                && vars.size() == 2) {
                            Iterator<LocalVariable> it = vars.iterator();
                            LocalVariable var1 = it.next();
                            LocalVariable var2 = it.next();

                            BasicBlock block1 = var1.getBasicBlock();
                            BasicBlock block2 = var2.getBasicBlock();
                            DominatorInfo<BasicBlock, Edge> dominatorInfo = forkBlock.getGraph().getDominatorInfo();
                            Edge forkEdge1 = dominatorInfo.getPostDominanceFrontierOf(block1).iterator().next();
                            Edge forkEdge2 = dominatorInfo.getPostDominanceFrontierOf(block2).iterator().next();
                            LocalVariable thenVar = null;
                            LocalVariable elseVar = null;
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
                                AnalysisData forkData = (AnalysisData) forkBlock.getData();
                                JumpIfInst jumpIfInst = (JumpIfInst) forkData.getLastInst();
                                choiceInst.getChoices().remove(thenVar);
                                choiceInst.getChoices().remove(elseVar);
                                if (choiceInst.getChoices().isEmpty()) {
                                    LocalVariable resultVar = choiceInst.getResult();
                                    ConditionalInst condInst
                                            = new ConditionalInst(resultVar, jumpIfInst.getCond(), thenVar, elseVar);
                                    logger.log(Level.FINER, "Replace inst at {0} of {1} : {2}",
                                            new Object[]{i, joinBlock, TACInstWriter.toText(condInst)});
                                    replacement.add(condInst);
                                } else {
                                    LocalVariable resultVar = tmpVarFactory.create(forkBlock);
                                    ConditionalInst condInst
                                            = new ConditionalInst(resultVar, jumpIfInst.getCond(), thenVar, elseVar);
                                    logger.log(Level.FINER, "Insert inst at {0} of {1} : {2}",
                                            new Object[]{i, joinBlock, TACInstWriter.toText(condInst)});
                                    replacement.add(condInst);
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

                if (replacement.size() > 0) {
                    joinInsts.remove(i);
                    joinInsts.addAll(i, replacement);
                }
            }
        }
    }


    public void build() {
//        for (BasicBlock block : graph.getBasicBlocks()) {
//            block.setData(new AnalysisData());
//        }

        tmpVarFactory = new TemporaryVariableFactory();

        List<BasicBlock> blocksToProcess = new ArrayList<BasicBlock>(graph.getDFST().getNodes());
        while (blocksToProcess.size() > 0) {
            for (Iterator<BasicBlock> it = blocksToProcess.iterator(); it.hasNext();) {
                BasicBlock block = it.next();

                boolean processable = true;
                for (Edge incomingEdge : graph.getIncomingEdgesOf(block)) {
                    if (incomingEdge.isLoopBack()) {
                        continue;
                    }
                    BasicBlock pred = graph.getEdgeSource(incomingEdge);
                    if (blocksToProcess.contains(pred)) {
                        processable = false;
                        break;
                    }
                }

                if (!processable) {
                    break;
                }

                List<ArrayDeque<LocalVariable>> inputStacks
                        = new ArrayList<ArrayDeque<LocalVariable>>();
                for (Edge incomingEdge : graph.getIncomingEdgesOf(block)) {
                    if (incomingEdge.isLoopBack()) {
                        continue;
                    }
                    BasicBlock pred = graph.getEdgeSource(incomingEdge);
                    AnalysisData data = (AnalysisData) pred.getData();
                    inputStacks.add(data.getOutputStack2().clone());
                }

                processBlock(block, inputStacks);
                it.remove();
            }
        }

        // replace choice instructions by conditional instructions (ternary operator)
        removeChoiceInst();

        // convert to SSA form
        new SSAFormConverter(graph).convert();

        // analyse local variables types
        new LocalVariableTypeAnalyser(graph, classNameFactory).analyse();
    }
}
