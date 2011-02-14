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

import fr.jamgotchian.abcd.core.ast.expr.Constant;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import java.util.ArrayList;
import java.util.HashMap;
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
public class ControlFlowGraphStmtAnalysis {

    private static final Logger logger = Logger.getLogger(ControlFlowGraphStmtAnalysis.class.getName());
    
    static {
        logger.setLevel(Level.FINE);
    }
    
    private ControlFlowGraph graph;
    
    private void processBlock(BasicBlock block, ExpressionStack inputStack) {

        logger.log(Level.FINER, "------ Process block {0} ------", block);

        BasicBlockAnalysisDataImpl data = (BasicBlockAnalysisDataImpl) block.getData();
        
        data.setInputStack(inputStack.clone());

        ExpressionStack outputStack = inputStack.clone();
        Iterator<Edge> itE = graph.getIncomingEdgesOf(block).iterator();
        if (itE.hasNext() && itE.next().isExceptional()) {
            Constant cst = new Constant("EXCEPTION");
            cst.setBasicBlock(block);
            outputStack.push(cst);
        }

        if (data.getInputStack().size() > 0) {
            logger.log(Level.FINEST, ">>> Input stack : {0}", data.getInputStack());
        }

        BasicBlockStmtAnalysis stmtsBuilder = new BasicBlockStmtAnalysis(outputStack);
        block.visit(stmtsBuilder);
        data.setOutputStack(outputStack);

        if (data.getOutputStack().size() > 0) {
            logger.log(Level.FINEST, "<<< Output stack : {0}", data.getOutputStack());
        }
    }
    
    public void analyse(ControlFlowGraph graph) {

        this.graph = graph;
  
        for (BasicBlock block : graph.getBasicBlocks()) {
            block.setData(new BasicBlockAnalysisDataImpl());
        }
        
        Map<BasicBlock, Integer> stackSizes = new StackSizeAnalysis(graph).analyse();
        for (Map.Entry<BasicBlock, Integer> entry : stackSizes.entrySet()) {
            BasicBlock block = entry.getKey();
            int stackSize = entry.getValue();
            for (Edge e : graph.getOutgoingEdgesOf(block)) {
                e.setStackSize(stackSize);
            }
        }

        for (Edge edge : graph.getEdges()) {
            logger.log(Level.FINEST, "Stack size at {0} : {1}",
                    new Object[] {graph.toString(edge), edge.getStackSize()});
        }

        Map<BasicBlock, Set<BasicBlock>> dependances = new HashMap<BasicBlock, Set<BasicBlock>>();
        for (BasicBlock block : graph.getBasicBlocks()) {
            dependances.put(block, new HashSet<BasicBlock>());
        }

        for (Edge edge : graph.getEdges()) {
            if (edge.getStackSize() > 0) {
                BasicBlock source = graph.getEdgeSource(edge);
                BasicBlock target = graph.getEdgeTarget(edge);
                dependances.get(target).add(source);
            }
        }
        
        for (Map.Entry<BasicBlock, Set<BasicBlock>> entry : dependances.entrySet()) {
            logger.log(Level.FINEST, "Stack dependency of {0} : {1}", 
                    new Object[] {entry.getKey(), entry.getValue()});
        }

        List<BasicBlock> blocksToProcess = new ArrayList<BasicBlock>(graph.getBasicBlocks());
        while (blocksToProcess.size() > 0) {
            for (Iterator<BasicBlock> it = blocksToProcess.iterator(); it.hasNext();) {
                BasicBlock block = it.next();
                
                boolean isProcessable = true;
                List<ExpressionStack> stacks = new ArrayList<ExpressionStack>();
                for (BasicBlock child : dependances.get(block)) {
                    if (blocksToProcess.contains(child)) {
                        isProcessable = false;
                        break;
                    } else {
                        stacks.add(((BasicBlockAnalysisDataImpl )child.getData()).getOutputStack().clone());
                    }
                }
                
                if (isProcessable) {
                    ExpressionStack inputStack = null;
                    if (stacks.isEmpty()) {
                        inputStack = new ExpressionStackImpl();
                    } else if (stacks.size() == 1) {
                        inputStack = stacks.get(0).clone();
                    } else {
                        inputStack = ExpressionStacks.merge(stacks);
                    }
                    processBlock(block, inputStack);
                    it.remove();
                }
            }
        }
    }
}
