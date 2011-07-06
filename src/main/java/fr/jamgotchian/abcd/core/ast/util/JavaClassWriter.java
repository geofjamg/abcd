/*
 *  Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHwriter ANY WARRANTY; withwriter even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.ast.util;

import fr.jamgotchian.abcd.core.ast.Class;
import fr.jamgotchian.abcd.core.ast.ClassVisitor;
import fr.jamgotchian.abcd.core.ast.Field;
import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.ast.stmt.StatementVisitor;
import fr.jamgotchian.abcd.core.output.CodeWriter;
import fr.jamgotchian.abcd.core.type.ClassName;
import java.util.List;
import javax.lang.model.element.Modifier;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaClassWriter implements ClassVisitor<Void, Void> {

    private final CodeWriter writer;

    private final boolean debug;

    private final StatementVisitor<Void, Void> stmtVisitor;

    public JavaClassWriter(CodeWriter writer, boolean debug) {
        this.writer = writer;
        this.debug = debug;
        this.stmtVisitor = new JavaStatementWriter(writer, debug);
    }

    public Void visit(Class _class, Void arg) {
        for (Modifier mod : _class.getModifiers()) {
            writer.write(mod).writeSpace();
        }
        writer.writeKeyword("class").writeSpace().write(_class.getName());
        if (_class.getSuperName() != null && !Object.class.getName().equals(_class.getSuperName())) {
            writer.incrIndent();
            writer.newLine().writeKeyword("extends").writeSpace().write(_class.getSuperName());
            writer.decrIndent();
        }

        if (_class.getInterfaces().size() > 0) {
            writer.incrIndent();
            writer.newLine().writeKeyword("implements");
            for (int i = 0; i < _class.getInterfaces().size(); i++) {
                String _interface = _class.getInterfaces().get(i);
                writer.writeSpace().write(_interface);
                if (i < _class.getInterfaces().size()-1) {
                    writer.write(",");
                }
            }
            writer.decrIndent();
        }
        writer.writeSpace().write("{").newLine().newLine();

        writer.incrIndent();
        for (Field field : _class.getFields()) {
            field.accept(this, null);
            writer.newLine();
        }

        for (Class innerClass : _class.getInnerClasses()) {
            visit(innerClass, null);
            writer.newLine();
        }

        for (Method method : _class.getMethods()) {
            method.accept(this, null);
            writer.newLine().newLine();
        }
        writer.decrIndent();

        writer.write("}").newLine();

        return null;
    }

    public Void visit(Field field, Void arg) {
        for (Modifier mod : field.getModifiers()) {
            writer.write(mod).writeSpace();
        }
        writer.write(field.getType()).writeSpace().write(field.getName()).write(";").newLine();
        return null;
    }

    public Void visit(Method method, Void _void) {
        for (Modifier mod : method.getModifiers()) {
            writer.write(mod).writeSpace();
        }
        if (!"<clinit>".equals(method.getName())) {
            if (method.getReturnType() != null) {
                writer.write(method.getReturnType()).writeSpace();
            }
            writer.write(method.getName()).write("(");
            List<LocalVariableDeclaration> arguments = method.getArguments();
            for (int i = 0; i < arguments.size(); i++) {
                LocalVariableDeclaration argument = arguments.get(i);
                writer.write(argument.getType()).writeSpace().write(argument.getVariable().getName());
                if (i < arguments.size()-1) {
                    writer.write(",").writeSpace();
                }
            }
            writer.write(")");
            writer.incrIndent();
            if (method.getExceptions().size() > 0) {
                writer.newLine().writeKeyword("throws").writeSpace();
                List<ClassName> exceptions = method.getExceptions();
                for (int i = 0; i < exceptions.size(); i++) {
                    writer.write(exceptions.get(i));
                    if (i < exceptions.size()-1) {
                        writer.write(", ");
                    }
                }
            }
            writer.writeSpace();
            writer.decrIndent();
        }
        method.getBody().accept(stmtVisitor, null);
        return null;
    }
}
