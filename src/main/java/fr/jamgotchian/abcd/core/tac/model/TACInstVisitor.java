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
package fr.jamgotchian.abcd.core.tac.model;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface TACInstVisitor<R, A> {

    R visit(TACInstSeq seq, A arg);

    R visit(LocalVariable inst, A arg);

    R visit(StaticField inst, A arg);

    R visit(IntConst inst, A arg);

    R visit(LongConst inst, A arg);

    R visit(ByteConst inst, A arg);

    R visit(ShortConst inst, A arg);

    R visit(FloatConst inst, A arg);

    R visit(DoubleConst inst, A arg);

    R visit(StringConst inst, A arg);

    R visit(NullConst inst, A arg);

    R visit(ClassConst inst, A arg);

    R visit(ArrayLengthInst inst, A arg);

    R visit(AssignInst inst, A arg);

    R visit(BinaryInst inst, A arg);

    R visit(CallMethodInst inst, A arg);

    R visit(CallStaticMethodInst inst, A arg);

    R visit(CastInst inst, A arg);

    R visit(ConditionalInst inst, A arg);

    R visit(GetArrayInst inst, A arg);

    R visit(SetArrayInst inst, A arg);

    R visit(GetFieldInst inst, A arg);

    R visit(SetFieldInst inst, A arg);

    R visit(GotoInst inst, A arg);

    R visit(JumpIfInst inst, A arg);

    R visit(LabelInst inst, A arg);

    R visit(InstanceOfInst inst, A arg);

    R visit(MonitorEnterInst inst, A arg);

    R visit(MonitorExitInst inst, A arg);

    R visit(NewArrayInst inst, A arg);

    R visit(NewObjectInst inst, A arg);

    R visit(ReturnInst inst, A arg);

    R visit(SwitchInst inst, A arg);

    R visit(ThrowInst inst, A arg);

    R visit(UnaryInst inst, A arg);

    R visit(ChoiceInst inst, A arg);

    R visit(PhiFunctionInst inst, A arg);
}
