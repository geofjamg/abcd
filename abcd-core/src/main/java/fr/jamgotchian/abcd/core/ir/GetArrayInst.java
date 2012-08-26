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

import com.google.common.collect.Sets;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class GetArrayInst extends DefInst {

    private final Variable array;

    private final Variable index;

    GetArrayInst(int defID, Variable result, Variable array, Variable index) {
        super(defID, result);
        this.array = array;
        this.index = index;
    }

    public Variable getArray() {
        return array;
    }

    public Variable getIndex() {
        return index;
    }

    @Override
    public Set<Variable> getUses() {
        return Sets.newHashSet(array, index);
    }

    @Override
    public <R, A> R accept(IRInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
