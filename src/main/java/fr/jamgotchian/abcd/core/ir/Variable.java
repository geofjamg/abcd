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

import fr.jamgotchian.abcd.core.type.ComputationalType;
import fr.jamgotchian.abcd.core.type.JavaType;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Variable {

    private final VariableID ID;

    private BasicBlock block;

    /* position in bytecode array */
    private final int position;

    private JavaType type;

    private ComputationalType computationalType;

    private String name = "???";

    public Variable(VariableID ID, BasicBlock block, int position) {
        this.ID = ID;
        this.block = block;
        this.position = position;
    }

    public Variable(Variable other) {
        ID = new VariableID(other.ID);
        block = other.block;
        position = other.position;
        type = other.type;
        computationalType = other.computationalType;
    }

    public VariableID getID() {
        return ID;
    }

    public int getIndex() {
        return ID.getIndex();
    }

    public int getVersion() {
        return ID.getVersion();
    }

    public void setVersion(int version) {
        ID.setVersion(version);
    }

    public BasicBlock getBasicBlock() {
        return block;
    }

    public void setBasicBlock(BasicBlock block) {
        this.block = block;
    }

    public int getPosition() {
        return position;
    }

    public boolean isThis() {
        return ID.getType() == VariableType.THIS;
    }

    public boolean isTemporary() {
        return ID.getType() == VariableType.TEMPORARY;
    }

    public JavaType getType() {
        return type;
    }

    public void setType(JavaType type) {
        this.type = type;
    }

    public ComputationalType getComputationalType() {
        return computationalType;
    }

    public void setComputationalType(ComputationalType computationalType) {
        this.computationalType = computationalType;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Variable)) {
            return false;
        }
        Variable var = (Variable) obj;
        return var.getID().equals(ID);
    }

    @Override
    public int hashCode() {
        return ID.hashCode();
    }

    @Override
    public String toString() {
        return ID.toString();
    }
}
