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
public class BinaryInst implements TACInst {

    private final Variable result;

    private final BinaryOp operator;

    private final Variable var1;

    private final Variable var2;

    public BinaryInst(Variable result, BinaryOp operator,
                      Variable var1, Variable var2) {
        this.result = result;
        this.operator = operator;
        this.var1 = var1;
        this.var2 = var2;
    }

    public Variable getResult() {
        return result;
    }

    public BinaryOp getOperator() {
        return operator;
    }

    public Variable getVar1() {
        return var1;
    }

    public Variable getVar2() {
        return var2;
    }

    public Variable getDef() {
        return result;
    }

    public Set<Variable> getUses() {
        return Sets.newHashSet(var1, var2);
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
