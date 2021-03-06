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

import fr.jamgotchian.abcd.core.bytecode.MethodFactory;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraphBuilder;
import fr.jamgotchian.abcd.core.ir.IRInstFactory;
import fr.jamgotchian.abcd.core.ir.InstructionBuilder;
import fr.jamgotchian.abcd.core.ir.VariableFactory;
import fr.jamgotchian.abcd.core.ir.Variable;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Modifier;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.MethodNode;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaBytecodeMethodFactory implements MethodFactory {

    private final ClassNode cn;

    private final MethodNode mn;

    private final LabelManager labelManager = new LabelManager();

    public JavaBytecodeMethodFactory(ClassNode cn, MethodNode mn) {
        this.cn = cn;
        this.mn = mn;
    }

    @Override
    public Method createMethod(ImportManager importManager, VariableFactory varFactory) {
        // return type
        JavaType javaReturnType = null;
        if (!"<init>".equals(mn.name)) {
            Type returnType = Type.getReturnType(mn.desc);
            javaReturnType = JavaBytecodeUtil.newType(returnType, importManager);
        }

        // method name
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

        // contructor or method ?
        boolean constructor = "<init>".equals(mn.name);

        // throwable exceptions
        List<ClassName> exceptions = new ArrayList<>();
        if (mn.exceptions != null) {
            for (String exception : (List<String>) mn.exceptions) {
                exceptions.add(importManager.newClassName(exception.replace('/', '.')));
            }
        }

        // method modifiers
        Set<Modifier> methodModifiers = JavaBytecodeUtil.getModifiers(mn.access);

        // parameters
        boolean isStatic = methodModifiers.contains(Modifier.STATIC);
        if (!isStatic) {
            varFactory.setThisIndex(0);
        }
        Type[] argTypes = Type.getArgumentTypes(mn.desc);
        List<Variable> arguments = new ArrayList<>(argTypes.length);
        for(int i = 0; i < argTypes.length; i++) {
            Type argType = argTypes[i];
            // index 0 of local variable table contains this for non static method
            int index = isStatic ? i : i + 1;
            varFactory.addArgIndex(index);
            Variable arg = varFactory.create(index);
            arg.setType(JavaBytecodeUtil.newType(argType, importManager));
            arguments.add(arg);
        }

        return new Method(methodName, methodModifiers, javaReturnType, arguments,
                          exceptions, constructor);
    }

    @Override
    public ControlFlowGraphBuilder createCFGBuilder(String methodSignature) {
        return new JavaBytecodeControlFlowGraphBuilder(methodSignature, mn, labelManager);
    }

    @Override
    public InstructionBuilder createInstBuilder(ImportManager importManager,
                                                VariableFactory varFactory,
                                                IRInstFactory instFactory) {
        return new JavaBytecodeInstructionBuilder(mn.instructions, labelManager,
                                                  importManager, varFactory, instFactory);
    }

    @Override
    public String getBytecodeAsText() {
        return JavaBytecodeWriter.toText(mn.instructions, labelManager);
    }
}
