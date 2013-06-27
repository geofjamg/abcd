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
package fr.jamgotchian.abcd.core.bytecode.java;

import fr.jamgotchian.abcd.core.bytecode.ClassFactory;
import fr.jamgotchian.abcd.core.bytecode.MethodFactory;
import fr.jamgotchian.abcd.core.ast.Field;
import fr.jamgotchian.abcd.core.ast.Package;
import fr.jamgotchian.abcd.core.ast.Class;
import fr.jamgotchian.abcd.core.ast.ClassKind;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Modifier;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.ClassReader;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaBytecodeClassFactory implements ClassFactory {

    private final ClassNode cn;

    public JavaBytecodeClassFactory(InputStream is) throws IOException {
        cn = new ClassNode();
        ClassReader cr = new ClassReader(is);
        cr.accept(cn, 0);
    }

    @Override
    public Class createClass(ImportManager importManager) {

        // class name
        ClassName className = importManager.newClassName(cn.name.replace('/', '.'));

        // package
        Package _package = new Package(className.getPackageName());

        // super class name
        ClassName superClassName = null;
        if (cn.superName != null) {
            superClassName = importManager.newClassName(cn.superName.replace('/', '.'));
        }

        // class kind
        ClassKind kind = JavaBytecodeUtil.getKind(cn.access);

        // class modifiers
        Set<Modifier> modifiers = JavaBytecodeUtil.getModifiers(cn.access);
        modifiers.remove(Modifier.SYNCHRONIZED); // ???

        // implemented interfaces
        List<ClassName> interfaceNames = new ArrayList<>();
        for (String _interface : (List<String>) cn.interfaces) {
            interfaceNames.add(importManager.newClassName(_interface.replace('/', '.')));
        }

        Class _class = new Class(_package, className, superClassName,
                                 interfaceNames, kind, modifiers);

        // fields
        for (FieldNode fn : (List<FieldNode>) cn.fields) {
            Type fieldType = Type.getType(fn.desc);
            JavaType javaFieldType = JavaBytecodeUtil.newType(fieldType, importManager);
            Expression valueExpr = null;
            if (fn.value != null) {
                if (fn.value instanceof Integer) {
                    valueExpr = Expressions.newIntExpr((Integer) fn.value);
                } else if (fn.value instanceof Long) {
                    valueExpr = Expressions.newLongExpr((Long) fn.value);
                } else if (fn.value instanceof Float) {
                    valueExpr = Expressions.newFloatExpr((Float) fn.value);
                } else if (fn.value instanceof Double) {
                    valueExpr = Expressions.newDoubleExpr((Double) fn.value);
                } else if (fn.value instanceof String) {
                    valueExpr = Expressions.newStringExpr((String) fn.value);
                } else {
                    throw new InternalError();
                }
            }

            _class.addField(new Field(JavaBytecodeUtil.getModifiers(fn.access),
                                      fn.name, javaFieldType, valueExpr));
        }
        return _class;
    }

    @Override
    public Collection<MethodFactory> createMethodFactories() {
        List<MethodFactory> factories = new ArrayList<>();
        for (MethodNode mn : (List<MethodNode>) cn.methods) {
            factories.add(new JavaBytecodeMethodFactory(cn, mn));
        }
        return factories;
    }
}
