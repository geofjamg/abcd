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

package fr.jamgotchian.abcd.core.ir;

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.util.Range;
import fr.jamgotchian.abcd.core.util.RangeImpl;
import java.util.EnumMap;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BasicBlockImpl implements BasicBlock {

    private final Range range;

    private BasicBlockType type;

    private final Object label;

    private int order;

    private IRInstSeq instructions;

    private VariableStack inputStack;

    private VariableStack outputStack;

    private VariableRegisters inputRegisters;

    private VariableRegisters outputRegisters;

    private Map<BasicBlockPropertyName, Object> properties;

    private ChildType childType;

    private Object data;

    public static BasicBlock createRange(int firstInstn, int lastInstn, BasicBlockType type) {
        Range range = new RangeImpl(firstInstn, lastInstn);
        return new BasicBlockImpl(range, type, range);
    }

    public static BasicBlock createEmpty() {
        return new BasicBlockImpl(null, BasicBlockType.EMPTY, "[]");
    }

    public static BasicBlock createEntry() {
        return new BasicBlockImpl(new RangeImpl(Integer.MIN_VALUE, -1), BasicBlockType.ENTRY, BasicBlockType.ENTRY);
    }

    public static BasicBlock createExit() {
        return new BasicBlockImpl(null, BasicBlockType.EXIT, BasicBlockType.EXIT);
    }

    public static BasicBlock createStop() {
        return new BasicBlockImpl(null, BasicBlockType.STOP, BasicBlockType.STOP);
    }

    protected BasicBlockImpl(Range range, BasicBlockType type, Object label) {
        this.range = range;
        this.type = type;
        this.label = label;
        order = -1;
        properties = new EnumMap<BasicBlockPropertyName, Object>(BasicBlockPropertyName.class);
        childType = ChildType.UNDEFINED;
    }

    @Override
    public Range getRange() {
        return range;
    }

    @Override
    public BasicBlockType getType() {
        return type;
    }

    @Override
    public void setType(BasicBlockType type) {
        this.type = type;
    }

    @Override
    public void setOrder(int order) {
        this.order = order;
    }

    @Override
    public int getOrder() {
        return order;
    }

    @Override
    public IRInstSeq getInstructions() {
        return instructions;
    }

    @Override
    public void setInstructions(IRInstSeq instructions) {
        this.instructions = instructions;
    }

    @Override
    public VariableStack getInputStack() {
        return inputStack;
    }

    @Override
    public void setInputStack(VariableStack inputStack) {
        this.inputStack = inputStack;
    }

    @Override
    public VariableStack getOutputStack() {
        return outputStack;
    }

    @Override
    public void setOutputStack(VariableStack outputStack) {
        this.outputStack = outputStack;
    }

    @Override
    public VariableRegisters getInputRegisters() {
        return inputRegisters;
    }

    @Override
    public void setInputRegisters(VariableRegisters inputRegisters) {
        this.inputRegisters = inputRegisters;
    }

    @Override
    public VariableRegisters getOutputRegisters() {
        return outputRegisters;
    }

    @Override
    public void setOutputRegisters(VariableRegisters outputRegisters) {
        this.outputRegisters = outputRegisters;
    }

    @Override
    public void putProperty(BasicBlockPropertyName name, Object value) {
        properties.put(name, value);
    }

    @Override
    public Object getProperty(BasicBlockPropertyName name) {
        return properties.get(name);
    }

    @Override
    public boolean hasProperty(BasicBlockPropertyName name) {
        return properties.containsKey(name);
    }

    @Override
    public Map<BasicBlockPropertyName, Object> getProperties() {
        return properties;
    }

    @Override
    public void putProperties(Map<BasicBlockPropertyName, Object> properties) {
        this.properties.putAll(properties);
    }

    @Override
    public BasicBlock getEntry() {
        return this;
    }

    @Override
    public BasicBlock getExit() {
        return this;
    }

    @Override
    public ParentType getParentType() {
        return ParentType.BASIC_BLOCK;
    }

    @Override
    public void setParentType(ParentType parentType) {
        throw new ABCDException("Cannot set parent type for a basic block region");
    }

    @Override
    public ChildType getChildType() {
        return childType;
    }

    @Override
    public void setChildType(ChildType childType) {
        this.childType = childType;
    }

    @Override
    public Object getData() {
        return data;
    }

    @Override
    public void setData(Object data) {
        this.data = data;
    }

    @Override
    public String toString() {
        return label.toString();
    }
}
