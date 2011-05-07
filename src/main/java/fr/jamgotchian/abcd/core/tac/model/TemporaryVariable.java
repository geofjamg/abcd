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

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TemporaryVariable extends Variable {

    private final int num;

    private final BasicBlock block;

    public TemporaryVariable(int num, BasicBlock block) {
        this.num = num;
        this.block = block;
    }

    public int getNum() {
        return num;
    }

    public BasicBlock getBasicBlock() {
        return block;
    }

    public boolean isTemporary() {
        return true;
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof TemporaryVariable)) {
            return false;
        }
        return ((TemporaryVariable) obj).num == num;
    }

    @Override
    public int hashCode() {
        return num;
    }
}
