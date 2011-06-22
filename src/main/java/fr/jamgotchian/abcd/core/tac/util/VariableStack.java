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
package fr.jamgotchian.abcd.core.tac.util;

import fr.jamgotchian.abcd.core.tac.model.Variable;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class VariableStack {

    private final ArrayDeque<Variable> stack;

    public VariableStack() {
        stack = new ArrayDeque<Variable>();
    }

    public VariableStack(VariableStack other) {
        stack = new ArrayDeque<Variable>(other.stack.size());
        for (Variable v : other.stack) {
            stack.add(v.clone());
        }
    }

    public void push(Variable v) {
        stack.push(v.clone());
    }

    public Variable pop() {
        return stack.pop().clone();
    }

    public int size() {
        return stack.size();
    }

    public List<Variable> toList() {
        return new ArrayList<Variable>(stack);
    }

    @Override
    public VariableStack clone() {
        return new VariableStack(this);
    }

    @Override
    public String toString() {
        return stack.toString();
    }
}
