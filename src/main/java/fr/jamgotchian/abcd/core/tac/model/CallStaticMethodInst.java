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

import fr.jamgotchian.abcd.core.type.ClassName;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CallStaticMethodInst implements TACInst {

    private final TemporaryVariable result;

    private final ClassName scope;

    private final String methodName;

    private final List<TemporaryVariable> args;

    public CallStaticMethodInst(TemporaryVariable result, ClassName scope, String methodName,
                                List<TemporaryVariable> args) {
        this.result = result;
        this.scope = scope;
        this.methodName = methodName;
        this.args = args;
    }

    public TemporaryVariable getResult() {
        return result;
    }

    public ClassName getScope() {
        return scope;
    }

    public String getMethodName() {
        return methodName;
    }

    public List<TemporaryVariable> getArgs() {
        return args;
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}