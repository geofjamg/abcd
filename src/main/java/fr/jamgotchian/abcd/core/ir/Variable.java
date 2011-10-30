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

import fr.jamgotchian.abcd.core.type.JavaType;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Variable {

    private final VariableID ID;

    private final BasicBlock block;

    /* position in bytecode array */
    private final int position;

    private JavaType type;

    private String name;

    Variable(VariableID ID, BasicBlock block, int position) {
        this.ID = ID;
        this.block = block;
        this.position = position;
    }

    public Variable(int index, BasicBlock block, int position) {
        this(new VariableID(index), block, position);
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

    public int getPosition() {
        return position;
    }

    public boolean isTemporary() {
        return ID.getIndex() < 0;
    }

    public JavaType getType() {
        return type;
    }

    public void setType(JavaType type) {
        this.type = type;
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
    public Variable clone() {
        return new Variable(ID.clone(), block, position);
    }

    @Override
    public String toString() {
        return ID.toString();
    }
}