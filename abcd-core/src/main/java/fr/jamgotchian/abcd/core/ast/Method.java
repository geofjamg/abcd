/*
 *  Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.ast;

import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ir.MethodContext;
import fr.jamgotchian.abcd.core.ir.Variable;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Modifier;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Method implements MethodContext {

    private Class _class;

    private final String name;

    private final Set<Modifier> modifiers;

    private final JavaType returnType;

    private final List<Variable> arguments;

    private final List<ClassName> exceptions;

    private final BlockStatement body;

    private final boolean constructor;

    public Method(String name, Set<Modifier> modifiers, JavaType returnType,
                  List<Variable> arguments, List<ClassName> exceptions,
                  boolean constructor) {
        this.name = name;
        this.modifiers = modifiers;
        this.returnType = returnType;
        this.arguments = arguments;
        this.exceptions = exceptions;
        this.body = new BlockStatement();
        this.constructor = constructor;
    }

    public Class getClazz() {
        return _class;
    }

    public void setClass(Class _class) {
        this._class = _class;
    }

    public String getName() {
        return name;
    }

    public String getSignature() {
        StringBuilder builder = new StringBuilder();
        builder.append(name).append("(");
        for (Iterator<Variable> it = arguments.iterator(); it.hasNext();) {
            Variable arg = it.next();
            builder.append(arg.getType().toString());
            if (it.hasNext()) {
                builder.append(",");
            }
        }
        builder.append(")");
        return builder.toString();
    }

    public Set<Modifier> getModifiers() {
        return modifiers;
    }

    @Override
    public JavaType getThisType() {
        return _class.getThisType();
    }

    @Override
    public JavaType getReturnType() {
        return returnType;
    }

    @Override
    public List<Variable> getArguments() {
        return arguments;
    }

    public List<ClassName> getExceptions() {
        return exceptions;
    }

    public boolean isConstructor() {
        return constructor;
    }

    public BlockStatement getBody() {
        return body;
    }

    public <R, A> R accept(ClassVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
