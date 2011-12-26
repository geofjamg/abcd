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

import fr.jamgotchian.abcd.core.graph.Orderable;
import fr.jamgotchian.abcd.core.util.Range;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface BasicBlock extends Orderable {

    Range getRange();

    BasicBlockType getType();

    void setType(BasicBlockType type);

    IRInstSeq getInstructions();

    void setInstructions(IRInstSeq instructions);

    VariableStack getInputStack();

    void setInputStack(VariableStack inputStack);

    VariableStack getOutputStack();

    void setOutputStack(VariableStack outputStack);

    VariableRegisters getInputRegisters();

    void setInputRegisters(VariableRegisters inputRegisters);

    VariableRegisters getOutputRegisters();

    void setOutputRegisters(VariableRegisters outputRegisters);

    Region getParent();

    void setParent(Region region);

    void putProperty(BasicBlockPropertyName name, Object value);

    Object getProperty(BasicBlockPropertyName name);

    boolean hasProperty(BasicBlockPropertyName name);

    Map<BasicBlockPropertyName, Object> getProperties();

    void putProperties(Map<BasicBlockPropertyName, Object> properties);
}
