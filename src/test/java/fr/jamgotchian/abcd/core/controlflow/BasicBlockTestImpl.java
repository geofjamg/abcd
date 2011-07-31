/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

package fr.jamgotchian.abcd.core.controlflow;

import fr.jamgotchian.abcd.core.graph.Vertex;
import fr.jamgotchian.abcd.core.util.Range;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BasicBlockTestImpl extends Vertex implements BasicBlock {

    private int order;

    private int loopLevel;

    private final TACInstSeq instructions;

    private VariableStack inputStack;

    private VariableStack outputStack;

    public BasicBlockTestImpl(int id) {
        this(null, id);
    }

    public BasicBlockTestImpl(String prefix, int id) {
        super(prefix, id);
        order = -1;
        loopLevel = 0;
        instructions = new TACInstSeq();
    }

    public Range getRange() {
        return null;
    }

    public ControlFlowGraph getGraph() {
        return null;
    }

    public void setGraph(ControlFlowGraph graph) {
    }

    public BasicBlockType getType() {
        return null;
    }

    public void setType(BasicBlockType type) {
    }

    public int getOrder() {
        return order;
    }

    public void setOrder(int order) {
        this.order = order;
    }

    public int getLoopLevel() {
        return loopLevel;
    }

    public void setLoopLevel(int loopLevel) {
        this.loopLevel = loopLevel;
    }

    public void visit(BasicBlockVisitor visitor) {
    }

    public TACInstSeq getInstructions() {
        return instructions;
    }

    public VariableStack getInputStack() {
        return inputStack;
    }

    public void setInputStack(VariableStack inputStack) {
        this.inputStack = inputStack;
    }

    public VariableStack getOutputStack() {
        return outputStack;
    }

    public void setOutputStack(VariableStack outputStack) {
        this.outputStack = outputStack;
    }

    public void resetState() {
        loopLevel = 0;
    }
}
