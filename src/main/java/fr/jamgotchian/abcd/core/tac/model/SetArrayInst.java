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
public class SetArrayInst implements TACInst {

    private final TemporaryVariable array;

    private final TemporaryVariable index;

    private final TemporaryVariable value;

    public SetArrayInst(TemporaryVariable array, TemporaryVariable index,
                        TemporaryVariable value) {
        this.array = array;
        this.index = index;
        this.value = value;
    }

    public TemporaryVariable getArray() {
        return array;
    }

    public TemporaryVariable getIndex() {
        return index;
    }

    public TemporaryVariable getValue() {
        return value;
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
