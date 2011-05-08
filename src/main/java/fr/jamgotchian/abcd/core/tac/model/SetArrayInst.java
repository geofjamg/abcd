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

import com.google.common.collect.Sets;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SetArrayInst implements TACInst {

    private final LocalVariable array;

    private final LocalVariable index;

    private final LocalVariable value;

    public SetArrayInst(LocalVariable array, LocalVariable index,
                        LocalVariable value) {
        this.array = array;
        this.index = index;
        this.value = value;
    }

    public LocalVariable getArray() {
        return array;
    }

    public LocalVariable getIndex() {
        return index;
    }

    public LocalVariable getValue() {
        return value;
    }

    public LocalVariable getDef() {
        return null;
    }

    public Set<LocalVariable> getUses() {
        return Sets.newHashSet(array, index, value);
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
