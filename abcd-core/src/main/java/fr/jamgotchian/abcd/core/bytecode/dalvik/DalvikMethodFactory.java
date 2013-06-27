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
package fr.jamgotchian.abcd.core.bytecode.dalvik;

import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.bytecode.MethodFactory;
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
import org.jf.dexlib.ClassDataItem.EncodedMethod;
import org.jf.dexlib.MethodIdItem;
import org.jf.dexlib.ProtoIdItem;
import org.jf.dexlib.TypeIdItem;
import org.jf.dexlib.Util.AccessFlags;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DalvikMethodFactory implements MethodFactory {

    private final EncodedMethod encodedMethod;

    private final CodeAddressManager addressManager;

    public DalvikMethodFactory(EncodedMethod encodedMethod) {
        this.encodedMethod = encodedMethod;
        addressManager = new CodeAddressManager(encodedMethod.codeItem.getInstructions());
    }

    @Override
    public Method createMethod(ImportManager importManager, VariableFactory varFactory) {
        MethodIdItem methodItem = encodedMethod.method;

        String methodName = methodItem.getMethodName().getStringValue();

        // contructor or method ?
        boolean constructor = "<init>".equals(methodName);

        // method modifiers
        AccessFlags[] accessFlags = AccessFlags.getAccessFlagsForClass(encodedMethod.accessFlags);
        Set<Modifier> modifiers = DalvikBytecodeUtil.getModifiers(accessFlags);

        ProtoIdItem prototype = methodItem.getPrototype();

        // return type
        JavaType returnType = DalvikBytecodeUtil.newType(prototype.getReturnType(), importManager);

        // parameters
        List<Variable> arguments = new ArrayList<>();
        if (prototype.getParameters() != null) {
            int argRegister = encodedMethod.codeItem.getRegisterCount()
                    - prototype.getParameterRegisterCount();
            for (TypeIdItem argItem : prototype.getParameters().getTypes()) {
                JavaType argType = DalvikBytecodeUtil.newType(argItem, importManager);
                varFactory.addArgIndex(argRegister);
                Variable arg = varFactory.create(argRegister);
                arg.setType(argType);
                arguments.add(arg);
                if (argType == JavaType.LONG || argType == JavaType.DOUBLE) {
                    argRegister += 2;
                } else {
                    argRegister++;
                }
            }
        }

        List<ClassName> exceptions = new ArrayList<>();

        return new Method(methodName, modifiers, returnType, arguments,
                          exceptions, constructor);
    }

    @Override
    public ControlFlowGraphBuilder createCFGBuilder(String methodSignature) {
        return new DalvikBytecodeControlFlowGraphBuilder(methodSignature,
                                                         encodedMethod.codeItem,
                                                         addressManager);
    }

    @Override
    public InstructionBuilder createInstBuilder(ImportManager importManager,
                                                VariableFactory varFactory,
                                                IRInstFactory instFactory) {
        return new DalvikBytecodeInstructionBuilder();
    }

    @Override
    public String getBytecodeAsText() {
        return DalvikBytecodeWriter.toText(encodedMethod.codeItem.getInstructions(),
                                           addressManager);
    }

}
