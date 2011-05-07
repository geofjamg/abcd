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

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LocalVariable extends Variable {

    private final int index;

    public LocalVariable(int index) {
        this.index = index;
    }

    public int getIndex() {
        return index;
    }

    public boolean isTemporary() {
        return false;
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof LocalVariable)) {
            return false;
        }
        return ((LocalVariable) obj).index == index;
    }

    @Override
    public int hashCode() {
        return index;
    }

    @Override
    public LocalVariable clone() {
        return new LocalVariable(index);
    }
}
