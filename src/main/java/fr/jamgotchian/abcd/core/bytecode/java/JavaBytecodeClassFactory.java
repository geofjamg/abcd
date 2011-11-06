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
import fr.jamgotchian.abcd.core.ast.ImportManager;
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

    public Class createClass(ImportManager importManager) {
        // package
        String packageName = "";
        int lastDotIndex = cn.name.lastIndexOf('/');
        if (lastDotIndex != -1) {
            packageName = cn.name.substring(0, lastDotIndex).replace('/', '.');
        }
        Package _package = new Package(packageName);

        // class name
        String className = cn.name.replace('/', '.');
        String simpleClassName = null;
        lastDotIndex = className.lastIndexOf('.');
        if (lastDotIndex != -1) {
            simpleClassName = className.substring(lastDotIndex+1);
        } else { // class is in default package
            simpleClassName = className;
        }

        // super class name
        ClassName superClassName = null;
        if (cn.superName != null) {
            superClassName = importManager.newClassName(cn.superName.replace('/', '.'));
        }

        // class modifiers
        Set<Modifier> classModifiers = JavaBytecodeUtil.getModifiers(cn.access);
        classModifiers.remove(Modifier.SYNCHRONIZED); // ???

        // implemented interfaces
        List<ClassName> interfaceNames = new ArrayList<ClassName>();
        for (String _interface : (List<String>) cn.interfaces) {
            interfaceNames.add(importManager.newClassName(_interface.replace('/', '.')));
        }

        Class _class = new Class(_package, simpleClassName, superClassName,
                                 interfaceNames, classModifiers);

        // fields
        for (FieldNode fn : (List<FieldNode>) cn.fields) {
            Type fieldType = Type.getType(fn.desc);
            JavaType javaFieldType = JavaBytecodeUtil.newType(fieldType, importManager);

            _class.addField(new Field(JavaBytecodeUtil.getModifiers(fn.access),
                                      fn.name,
                                      javaFieldType));
        }
        return _class;
    }

    public Collection<MethodFactory> createMethodFactories() {
        List<MethodFactory> factories = new ArrayList<MethodFactory>();
        for (MethodNode mn : (List<MethodNode>) cn.methods) {
            factories.add(new JavaBytecodeMethodFactory(cn, mn));
        }
        return factories;
    }
}
