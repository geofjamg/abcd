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

import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.tac.AssignInst;
import fr.jamgotchian.abcd.core.tac.ChoiceInst;
import fr.jamgotchian.abcd.core.tac.StringConst;
import fr.jamgotchian.abcd.core.tac.TemporaryVariable;
import fr.jamgotchian.abcd.core.tac.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.output.OutputUtil;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TreeAddressCodeBuilder {

    private static final Logger logger = Logger.getLogger(TreeAddressCodeBuilder.class.getName());

    private ControlFlowGraph graph;

    private ImportManager importManager;

    private TemporaryVariableFactory tmpVarFactory;

    private void processBlock(BasicBlock block, ArrayDeque<TemporaryVariable> inputStack) {

        logger.log(Level.FINER, "------ Process block {0} ------", block);

        AnalysisData data = (AnalysisData) block.getData();

        data.setInputStack2(inputStack.clone());

        ArrayDeque<TemporaryVariable> outputStack = inputStack.clone();
        Iterator<Edge> itE = graph.getIncomingEdgesOf(block).iterator();
        if (itE.hasNext() && itE.next().isExceptional()) {
            TemporaryVariable tmpVar = tmpVarFactory.create(block);
            BasicBlock3ACBuilder.addInst(block, new AssignInst(tmpVar, new StringConst("EXCEPTION")));
            outputStack.push(tmpVar);
        }

        if (data.getInputStack2().size() > 0) {
            logger.log(Level.FINEST, ">>> Input stack : {0}",
                    OutputUtil.toText2(data.getInputStack2()));
        }

        BasicBlock3ACBuilder builder = new BasicBlock3ACBuilder(importManager, tmpVarFactory, outputStack);
        block.visit(builder);
        data.setOutputStack2(outputStack);

        if (data.getOutputStack().size() > 0) {
            logger.log(Level.FINEST, "<<< Output stack : {0}",
                    OutputUtil.toText2(data.getOutputStack2()));
        }
    }

    public void build(ControlFlowGraph graph, ImportManager importManager) {

        this.graph = graph;
        this.importManager = importManager;
//        for (BasicBlock block : graph.getBasicBlocks()) {
//            block.setData(new BasicBlockAnalysisDataImpl());
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

                List<ArrayDeque<TemporaryVariable>> stacks = new ArrayList<ArrayDeque<TemporaryVariable>>();
                for (Edge incomingEdge : graph.getIncomingEdgesOf(block)) {
                    if (incomingEdge.isLoopBack()) {
                        continue;
                    }
                    BasicBlock pred = graph.getEdgeSource(incomingEdge);
                    AnalysisData data = (AnalysisData) pred.getData();
                    stacks.add(data.getOutputStack2().clone());
                }

                ArrayDeque<TemporaryVariable> inputStack = null;
                if (stacks.isEmpty()) {
                    inputStack = new ArrayDeque<TemporaryVariable>();
                } else if (stacks.size() == 1) {
                    inputStack = stacks.get(0).clone();
                } else {
                    inputStack = mergeStacks(stacks, block);
                }

                processBlock(block, inputStack);
                it.remove();
            }
        }
    }

    private ArrayDeque<TemporaryVariable> mergeStacks
            (List<ArrayDeque<TemporaryVariable>> stacks, BasicBlock block) {
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

        ArrayDeque<TemporaryVariable> stacksMerge
                = new ArrayDeque<TemporaryVariable>(stacks.get(0).size());

        List<List<TemporaryVariable>> toList
                = new ArrayList<List<TemporaryVariable>>(stacks.size());
        for (int i = 0; i < stacks.size(); i++) {
            toList.add(new ArrayList<TemporaryVariable>(stacks.get(i)));
        }
        for (int i = 0; i < stacks.get(0).size(); i++) {
            Set<TemporaryVariable> vars = new HashSet<TemporaryVariable>(stacks.size());
            for (int j = 0; j < stacks.size(); j++) {
                vars.add(toList.get(j).get(i));
            }
            if (vars.size() == 1) {
                stacksMerge.push(vars.iterator().next());
            } else {
                TemporaryVariable result = tmpVarFactory.create(block);
                BasicBlock3ACBuilder.addInst(block, new ChoiceInst(result, vars));
                stacksMerge.push(result);
            }
        }

        return stacksMerge;
    }
}
