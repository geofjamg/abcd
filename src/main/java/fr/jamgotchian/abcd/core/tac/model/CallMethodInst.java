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

import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CallMethodInst implements TACInst {

    private final LocalVariable result;

    private final LocalVariable object;

    private final String methodName;

    private final JavaType returnType;

    private final List<JavaType> argTypes;

    private final List<LocalVariable> args;

    public CallMethodInst(LocalVariable result, LocalVariable object,
                          String methodName, JavaType returnType,
                          List<JavaType> argTypes, List<LocalVariable> args) {
        this.result = result;
        this.object = object;
        this.methodName = methodName;
        this.returnType = returnType;
        this.argTypes = argTypes;
        this.args = args;
    }

    public LocalVariable getResult() {
        return result;
    }

    public LocalVariable getObject() {
        return object;
    }

    public String getMethodName() {
        return methodName;
    }

    public JavaType getReturnType() {
        return returnType;
    }

    public List<JavaType> getArgTypes() {
        return argTypes;
    }

    public List<LocalVariable> getArgs() {
        return args;
    }

    public LocalVariable getDef() {
        return result;
    }

    public Set<LocalVariable> getUses() {
        Set<LocalVariable> uses = new HashSet<LocalVariable>(args.size()+1);
        uses.add(object);
        uses.addAll(args);
        return uses;
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
