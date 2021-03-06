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
package fr.jamgotchian.abcd.core.ir;

import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IRInstFactory {

    private int defID = 0;

    private int cstID = 0;

    public IRInstFactory() {
    }

    public ArrayLengthInst newArrayLength(Variable result, Variable array) {
        return new ArrayLengthInst(defID++, result, array);
    }

    public AssignConstInst newAssignInt(Variable result, int value) {
        return new AssignConstInst(defID++, result, new IntConst(cstID++, value));
    }

    public AssignConstInst newAssignByte(Variable result, byte value) {
        return new AssignConstInst(defID++, result, new ByteConst(cstID++, value));
    }

    public AssignConstInst newAssignShort(Variable result, short value) {
        return new AssignConstInst(defID++, result, new ShortConst(cstID++, value));
    }

    public AssignConstInst newAssignLong(Variable result, long value) {
        return new AssignConstInst(defID++, result, new LongConst(cstID++, value));
    }

    public AssignConstInst newAssignFloat(Variable result, float value) {
        return new AssignConstInst(defID++, result, new FloatConst(cstID++, value));
    }

    public AssignConstInst newAssignDouble(Variable result, double value) {
        return new AssignConstInst(defID++, result, new DoubleConst(cstID++, value));
    }

    public AssignConstInst newAssignString(Variable result, String value) {
        return new AssignConstInst(defID++, result, new StringConst(cstID++, value));
    }

    public AssignConstInst newAssignClass(Variable result, ClassName value) {
        return new AssignConstInst(defID++, result, new ClassConst(cstID++, value));
    }

    public AssignConstInst newAssignNull(Variable result) {
        return new AssignConstInst(defID++, result, new NullConst(cstID++));
    }

    public AssignVarInst newAssignVar(Variable result, Variable value) {
        return new AssignVarInst(defID++, result, value);
    }

    public BinaryInst newBinary(Variable result, IRBinaryOperator operator,
                                Variable var1, Variable var2) {
        return new BinaryInst(defID++, result, operator, var1, var2);
    }

    public CallMethodInst newCallMethod(Variable result, Variable object,
                                        MethodSignature signature,
                                        List<Variable> arguments) {
        return new CallMethodInst(defID++, result, object, signature, arguments);
    }

    public CallStaticMethodInst newCallStaticMethod(Variable result, ClassName scope,
                                                    MethodSignature signature,
                                                    List<Variable> arguments) {
        return new CallStaticMethodInst(defID++, result, scope, signature, arguments);
    }

    public CastInst newCast(Variable result, Variable var, JavaType type) {
        return new CastInst(defID++, result, var, type);
    }

    public ChoiceInst newChoice(Variable result, Set<Variable> choices) {
        return new ChoiceInst(defID++, result, choices);
    }

    public ConditionalInst newConditional(Variable result, Variable cond,
                                          Variable then, Variable _else) {
        return new ConditionalInst(defID++, result, cond, then, _else);
    }

    public GetArrayInst newGetArray(Variable result, Variable array, Variable index) {
        return new GetArrayInst(defID++, result, array, index);
    }

    public GetFieldInst newGetField(Variable result, Variable object,
                                    String fieldName, JavaType fieldType) {
        return new GetFieldInst(defID++, result, object, fieldName, fieldType);
    }

    public GetStaticFieldInst newGetStaticField(Variable result, ClassName scope,
                                                String fieldName, JavaType fieldType) {
        return new GetStaticFieldInst(defID++, result, scope, fieldName, fieldType);
    }

    public InstanceOfInst newInstanceOf(Variable result, Variable var, JavaType type) {
        return new InstanceOfInst(defID++, result, var, type);
    }

    public JumpIfInst newJumpIf(Variable cond) {
        return new JumpIfInst(cond);
    }

    public MonitorEnterInst newMonitorEnter(Variable var) {
        return new MonitorEnterInst(var);
    }

    public MonitorExitInst newMonitorExit(Variable var) {
        return new MonitorExitInst(var);
    }

    public NewArrayInst newNewArray(Variable result, JavaType type,
                                    List<Variable> dimensions) {
        return new NewArrayInst(defID++, result, type, dimensions);
    }

    public NewObjectInst newNewObject(Variable result, JavaType type) {
        return new NewObjectInst(defID++, result, type);
    }

    public PhiInst newPhi(Variable result, List<Variable> args) {
        return new PhiInst(defID++, result, args);
    }

    public ReturnInst newReturn(Variable var) {
        return new ReturnInst(var);
    }

    public ReturnInst newReturn() {
        return new ReturnInst();
    }

    public SetArrayInst newSetArray(Variable array, Variable index, Variable value) {
        return new SetArrayInst(array, index, value);
    }

    public SetFieldInst newSetField(Variable object, String fieldName, JavaType fieldType,
                                    Variable value) {
        return new SetFieldInst(object, fieldName, fieldType, value);
    }

    public SetStaticFieldInst newSetStaticField(ClassName scope, String fieldName,
                                                JavaType fieldType, Variable value) {
        return new SetStaticFieldInst(scope, fieldName, fieldType, value);
    }

    public SwitchInst newSwitch(Variable index) {
        return new SwitchInst(index);
    }

    public ThrowInst newThrow(Variable var) {
        return new ThrowInst(var);
    }

    public UnaryInst newUnary(Variable result, IRUnaryOperator operator, Variable var) {
        return new UnaryInst(defID++, result, operator, var);
    }
}
