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

    private int order;

    private IRInstSeq instructions;

    private VariableStack inputStack;

    private VariableStack outputStack;

    private VariableRegisters inputRegisters;

    private VariableRegisters outputRegisters;

    private Region region;

    private Map<BasicBlockPropertyName, Object> properties;

    public BasicBlockImpl(Range range, BasicBlockType type) {
        this.range = range;
        this.type = type;
        order = -1;
        properties = new EnumMap<BasicBlockPropertyName, Object>(BasicBlockPropertyName.class);
    }

    public BasicBlockImpl(int firstInstn, int lastInstn, BasicBlockType type) {
        this(new RangeImpl(firstInstn, lastInstn), type);
    }

    public BasicBlockImpl(BasicBlockType type) {
        this(null, type);
    }

    public BasicBlockImpl() {
        this(null, null);
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
    public Region getRegion() {
        return region;
    }

    @Override
    public void setRegion(Region region) {
        this.region = region;
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
    public String toString() {
        if (type != null) {
            switch (type) {
                case ENTRY:
                case EXIT:
                    return type.name();
            }
        }
        return range == null ? "[]" : range.toString();
    }
}
