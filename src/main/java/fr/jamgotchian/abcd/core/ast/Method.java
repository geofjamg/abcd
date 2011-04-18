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
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.ast.type.ClassName;
import fr.jamgotchian.abcd.core.ast.type.JavaType;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Modifier;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Method {

    private final String name;

    private final Set<Modifier> modifiers;

    private final JavaType returnType;

    private final List<LocalVariableDeclaration> arguments;
        
    private final List<ClassName> exceptions;

    private final BlockStatement body;

    private final boolean constructor;
    
    public Method(String name, Set<Modifier> modifiers, JavaType returnType, 
                    List<LocalVariableDeclaration> arguments, List<ClassName> exceptions, 
                    boolean constructor) {
        this.name = name;
        this.modifiers = modifiers;
        this.returnType = returnType;
        this.arguments = arguments;
        this.exceptions = exceptions;
        this.body = new BlockStatement();
        this.constructor = constructor;

        // fill local variable table
        if(!this.modifiers.contains(Modifier.STATIC)) {
            body.getLocalVariableTable().addVariable(0, "this");
        }
        for (LocalVariableDeclaration argument : this.arguments) {
            String argumentName = body.getNameGenerator().generate();
            body.getLocalVariableTable().addVariable(argument.getIndex(), argumentName);
        }
    }

    public String getName() {
        return name;
    }

    public Set<Modifier> getModifiers() {
        return modifiers;
    }

    public JavaType getReturnType() {
        return returnType;
    }

    public List<LocalVariableDeclaration> getArguments() {
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
