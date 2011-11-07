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

import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Modifier;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaBytecodeUtil implements Opcodes {

    private JavaBytecodeUtil() {
    }

    public static Set<Modifier> getModifiers(int access) {
        Set<Modifier> modifiers = EnumSet.noneOf(Modifier.class);

        if ((access & ACC_ABSTRACT) != 0) {
            modifiers.add(Modifier.ABSTRACT);
        }
        if ((access & ACC_FINAL) != 0) {
            modifiers.add(Modifier.FINAL);
        }
        if ((access & ACC_NATIVE) != 0) {
            modifiers.add(Modifier.NATIVE);
        }
        if ((access & ACC_PRIVATE) != 0) {
            modifiers.add(Modifier.PRIVATE);
        }
        if ((access & ACC_PROTECTED) != 0) {
            modifiers.add(Modifier.PROTECTED);
        }
        if ((access & ACC_PUBLIC) != 0) {
            modifiers.add(Modifier.PUBLIC);
        }
        if ((access & ACC_STATIC) != 0) {
            modifiers.add(Modifier.STATIC);
        }
        if ((access & ACC_SYNCHRONIZED) != 0) {
            modifiers.add(Modifier.SYNCHRONIZED);
        }
        if ((access & ACC_TRANSIENT) != 0) {
            modifiers.add(Modifier.TRANSIENT);
        }
        if ((access & ACC_VOLATILE) != 0) {
            modifiers.add(Modifier.VOLATILE);
        }
        return modifiers;
    }

    /**
     * Convert from ASM type to ABCD type
     * @param type ASM type
     * @param factory <code>ClassName</code> factory
     * @return ABCD type
     */
    public static JavaType newType(Type type, ClassNameFactory factory) {
        switch (type.getSort()) {
            case Type.VOID:
                return JavaType.VOID;
            case Type.BOOLEAN:
                return JavaType.BOOLEAN;
            case Type.CHAR:
                return JavaType.CHAR;
            case Type.BYTE:
                return JavaType.BYTE;
            case Type.SHORT:
                return JavaType.SHORT;
            case Type.INT:
                return JavaType.INT;
            case Type.FLOAT:
                return JavaType.FLOAT;
            case Type.LONG:
                return JavaType.LONG;
            case Type.DOUBLE:
                return JavaType.DOUBLE;
            case Type.ARRAY:
                return JavaType.newArrayType(newType(type.getElementType(), factory), type.getDimensions());
            case Type.OBJECT: {
                ClassName argClassName = factory.newClassName(type.getClassName());
                return JavaType.newRefType(argClassName);
            }
            default:
                throw new InternalError();
        }
    }

    public static void printInnerClasses(Multimap<String, String> innerClasses,
                                         StringBuilder builder) {
        List<String> outerClassColumn = new ArrayList<String>();
        List<String> innerClassColumn = new ArrayList<String>();
        outerClassColumn.add("Outer class");
        innerClassColumn.add("Inner class");
        for (Map.Entry<String, Collection<String>> entry : innerClasses.asMap().entrySet()) {
            String outerClass = entry.getKey();
            List<String> innerClasses2 = new ArrayList<String>(entry.getValue());
            outerClassColumn.add(outerClass);
            innerClassColumn.add(innerClasses2.get(0));
            for (int i = 1; i < innerClasses2.size(); i++) {
                outerClassColumn.add("");
                innerClassColumn.add(innerClasses2.get(i));
            }
        }
        ConsoleUtil.printTable(builder, outerClassColumn, innerClassColumn);
    }
}