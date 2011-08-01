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

import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockType;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeAttribute;
import fr.jamgotchian.abcd.core.controlflow.StringConst;
import fr.jamgotchian.abcd.core.controlflow.Variable;
import fr.jamgotchian.abcd.core.controlflow.TACInst;
import fr.jamgotchian.abcd.core.controlflow.TACInstFactory;
import fr.jamgotchian.abcd.core.controlflow.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.controlflow.VariableStack;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
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

    private static final Logger logger
            = Logger.getLogger(TreeAddressCodeBuilder.class.getName());

    private final ControlFlowGraph graph;

    private final Method method;

    private final ClassNameFactory classNameFactory;

    private final TemporaryVariableFactory tmpVarFactory;

    private final TACInstFactory instFactory;

    public TreeAddressCodeBuilder(ControlFlowGraph graph, Method method,
                                  ClassNameFactory classNameFactory,
                                  TemporaryVariableFactory tmpVarFactory,
                                  TACInstFactory instFactory) {
        this.graph = graph;
        this.method = method;
        this.classNameFactory = classNameFactory;
        this.tmpVarFactory = tmpVarFactory;
        this.instFactory = instFactory;
    }

    private void processBlock(BasicBlock block, List<VariableStack> inputStacks) {

        logger.log(Level.FINER, "------ Process block {0} ------", block);

        VariableStack inputStack = null;
        if (inputStacks.isEmpty()) {
            inputStack = new VariableStack();
        } else if (inputStacks.size() == 1) {
            inputStack = inputStacks.get(0).clone();
        } else {
            inputStack = mergeStacks(inputStacks, block);
        }

        block.setInputStack(inputStack.clone());

        VariableStack outputStack = inputStack.clone();

        if (block.getType() != BasicBlockType.EMPTY) {
            Edge edge = graph.getFirstIncomingEdgeOf(block);
            if (edge != null && edge.isExceptional()) {
                Variable tmpVar = tmpVarFactory.create(block);
                TACInst tmpInst;
                if (edge.getValue() == null) { // finally
                    tmpInst = instFactory.newAssignConst(tmpVar, new StringConst("TMP", classNameFactory));
                } else { // catch
                    ClassName className = classNameFactory.newClassName((String) edge.getValue());
                    tmpInst = instFactory.newNewObject(tmpVar, JavaType.newRefType(className));
                }
                BasicBlock3ACBuilder.addInst(block, tmpInst);
                outputStack.push(tmpVar);
            }
        }

        if (block.getInputStack().size() > 0) {
            logger.log(Level.FINEST, ">>> Input stack : {0}", block.getInputStack());
        }

        BasicBlock3ACBuilder builder
                = new BasicBlock3ACBuilder(classNameFactory, tmpVarFactory,
                                           outputStack, instFactory);
        block.visit(builder);
        block.setOutputStack(outputStack);

        if (block.getOutputStack().size() > 0) {
            logger.log(Level.FINEST, "<<< Output stack : {0}", block.getOutputStack());
        }
    }

    private VariableStack mergeStacks(List<VariableStack> stacks, BasicBlock block) {
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

        VariableStack stacksMerge = new VariableStack();

        List<List<Variable>> toList = new ArrayList<List<Variable>>(stacks.size());
        for (int i = 0; i < stacks.size(); i++) {
            toList.add(stacks.get(i).toList());
        }
        for (int i = 0; i < stacks.get(0).size(); i++) {
            Set<Variable> vars = new HashSet<Variable>(stacks.size());
            for (int j = 0; j < stacks.size(); j++) {
                vars.add(toList.get(j).get(i));
            }
            if (vars.size() == 1) {
                stacksMerge.push(vars.iterator().next());
            } else {
                Variable result = tmpVarFactory.create(block);
                BasicBlock3ACBuilder.addInst(block, instFactory.newChoice(result, vars));
                stacksMerge.push(result);
            }
        }

        return stacksMerge;
    }

    public void build() {
        List<BasicBlock> blocksToProcess = new ArrayList<BasicBlock>(graph.getDFST().getNodes());
        while (blocksToProcess.size() > 0) {
            for (Iterator<BasicBlock> it = blocksToProcess.iterator(); it.hasNext();) {
                BasicBlock block = it.next();

                List<VariableStack> inputStacks = new ArrayList<VariableStack>();
                for (Edge incomingEdge : graph.getIncomingEdgesOf(block)) {
                    if (incomingEdge.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
                        continue;
                    }
                    BasicBlock pred = graph.getEdgeSource(incomingEdge);
                    inputStacks.add(pred.getOutputStack().clone());
                }

                processBlock(block, inputStacks);
                it.remove();
            }
        }
    }
}
