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
package fr.jamgotchian.abcd.core.tac.model;

import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.type.JavaType;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LocalVariable implements Operand {

    private final LocalVariableID ID;

    private final BasicBlock block;

    private JavaType type;

    public LocalVariable(LocalVariableID ID, BasicBlock block) {
        this.ID = ID;
        this.block = block;
    }

    public LocalVariable(int index, BasicBlock block) {
        this(new LocalVariableID(index), block);
    }

    public LocalVariableID getID() {
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

    public boolean isTemporary() {
        return ID.getIndex() < 0;
    }

    public JavaType getType() {
        return type;
    }

    public void setType(JavaType type) {
        this.type = type;
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof LocalVariable)) {
            return false;
        }
        LocalVariable var = (LocalVariable) obj;
        return var.getID().equals(ID);
    }

    @Override
    public int hashCode() {
        return ID.hashCode();
    }

    @Override
    public LocalVariable clone() {
        return new LocalVariable(ID.clone(), block);
    }

    @Override
    public String toString() {
        return ID.toString();
    }
}
