/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

package fr.jamgotchian.abcd.core.util;

import java.util.EnumSet;
import java.util.Set;
import javax.lang.model.element.Modifier;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.MethodNode;


/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ASMUtil implements Opcodes {

    private ASMUtil() {
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

    public static String getMethodName(ClassNode cn, MethodNode mn) {
        String methodName;
        if ("<init>".equals(mn.name)) {
            String className = cn.name.replace('/', '.');
            int lastDotIndex = className.lastIndexOf('.');
            if (lastDotIndex != -1) {
                methodName = className.substring(lastDotIndex+1);
            } else {
                methodName = className;
            }
        } else {
            methodName = mn.name;
        }
        return methodName;
    }

    public static String getMethodSignature(ClassNode cn, MethodNode mn) {
        String methodName = ASMUtil.getMethodName(cn, mn);
        // build method signature
        StringBuilder methodSignature = new StringBuilder(methodName);
        methodSignature.append('(');
        Type[] argumentTypes = Type.getArgumentTypes(mn.desc);
        for (int i = 0; i < argumentTypes.length; i++) {
            methodSignature.append(argumentTypes[i].getClassName());
            if (i < argumentTypes.length-1) {
                methodSignature.append(',');
            }
        }
        methodSignature.append(')');
        return methodSignature.toString();
    }

    public static String getReturnTypeName(MethodNode mn) {
        if ("<init>".equals(mn.name)) {
            return "";
        } else {
            Type returnType = Type.getReturnType(mn.desc);
            return returnType.getClassName();
        }
    }
}
