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
import fr.jamgotchian.abcd.core.ir.TemporaryVariableFactory;
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
import org.jf.dexlib.Util.AccessFlags;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DalvikMethodFactory implements MethodFactory {

    private final EncodedMethod encodedMethod;

    public DalvikMethodFactory(EncodedMethod encodedMethod) {
        this.encodedMethod = encodedMethod;
    }

    public Method createMethod(ImportManager importManager) {
        MethodIdItem methodItem = encodedMethod.method;
        String name = methodItem.getMethodName().getStringValue();
        AccessFlags[] accessFlags = AccessFlags.getAccessFlagsForClass(encodedMethod.accessFlags);
        Set<Modifier> modifiers = DalvikBytecodeUtil.getModifiers(accessFlags);
        ProtoIdItem prototype = methodItem.getPrototype();
        JavaType returnType = DalvikBytecodeUtil.newType(prototype.getReturnType(), importManager);
        List<Variable> arguments = new ArrayList<Variable>();
        List<ClassName> exceptions = new ArrayList<ClassName>();
        return new Method(name, modifiers, returnType, arguments, exceptions, false);
    }

    public ControlFlowGraphBuilder createCFGBuilder(String methodSignature) {
        return new DalvikBytecodeControlFlowGraphBuilder(methodSignature);
    }

    public InstructionBuilder createInstBuilder(ImportManager importManager,
                                                TemporaryVariableFactory tmpVarFactory,
                                                IRInstFactory instFactory) {
        return new DalvikBytecodeInstructionBuilder();
    }

    public String getBytecodeAsText() {
        return "TODO";
    }

}
