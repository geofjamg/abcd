/*
 * Copyright (C) 2011 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *  *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package fr.jamgotchian.abcd.core.ir;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class VariableMapping {

    /* Variable ID mapping between temporary variables of the 2 sequences */
    private final Map<VariableID, VariableID> mapping
            = new HashMap<VariableID, VariableID>();

    public boolean defEqual(Variable var1, Variable var2) {
        if (mapping.containsKey(var1.getID())
                || mapping.containsKey(var2.getID())) {
            throw new IllegalStateException("Variables " + var1 + " or " + var2
                    + " already defined");
        }
        mapping.put(var1.getID(), var2.getID());
        mapping.put(var2.getID(), var1.getID());
        return true;
    }

    public boolean useEqual(Variable var1, Variable var2) {
        if (var1.getID().equals(var2.getID())) {
            return true;
        }
        return var2.getID().equals(mapping.get(var1.getID()));
    }

    public boolean usesEqual(List<Variable> vars1, List<Variable> vars2) {
        if (vars1.size() != vars2.size()) {
            return false;
        }
        for (int i = 0; i < vars1.size(); i++) {
            if (!useEqual(vars1.get(i), vars2.get(i))) {
                return false;
            }
        }
        return true;
    }

    public boolean usesEqual(Set<Variable> vars1, Set<Variable> vars2) {
        if (vars1.size() != vars2.size()) {
            return false;
        }
        for (Variable var1 : vars1) {
            boolean found = false;
            for (Variable var2 : vars2) {
                if (useEqual(var1, var2)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return true;
    }
}
