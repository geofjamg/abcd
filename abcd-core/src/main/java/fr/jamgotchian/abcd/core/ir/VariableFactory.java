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

import java.util.Set;
import java.util.TreeSet;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class VariableFactory {

    private int _thisIndex = -1;

    private final Set<Integer> argIndexes = new TreeSet<>();

    private int count = -1;

    public void setThisIndex(int _thisIndex) {
        this._thisIndex = _thisIndex;
    }

    public void addArgIndex(int argIndex) {
        argIndexes.add(argIndex);
    }

    public Set<Integer> getArgIndexes() {
        return argIndexes;
    }

    public Variable createTmp(BasicBlock block) {
        return new Variable(new VariableID(count--, VariableType.TEMPORARY), block, -1);
    }

    public Variable create(int index, BasicBlock block, int position) {
        VariableType type = null;
        if (_thisIndex != -1 && index == _thisIndex) {
            type = VariableType.THIS;
        } else if (argIndexes.contains(index)) {
            type = VariableType.ARGUMENT;
        } else {
            type = VariableType.LOCAL;
        }
        return new Variable(new VariableID(index, type), block, position);
    }

    public Variable create(int index) {
        return create(index, null, -1);
    }
}
