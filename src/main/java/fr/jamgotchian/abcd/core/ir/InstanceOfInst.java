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

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.Collections;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class InstanceOfInst extends DefInst {

    private final Variable var;

    private final JavaType type;

    InstanceOfInst(int defID, Variable result, Variable var, JavaType type) {
        super(defID, result);
        if (type.isPrimitive()) {
            throw new ABCDException("type.isPrimitive()");
        }
        this.var = var;
        this.type = type;
    }

    public Variable getVar() {
        return var;
    }

    public JavaType getType() {
        return type;
    }

    public Set<Variable> getUses() {
        return Collections.singleton(var);
    }

    public <R, A> R accept(IRInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
