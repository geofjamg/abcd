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
package fr.jamgotchian.abcd.core.tac.util;

import fr.jamgotchian.abcd.core.tac.model.ArrayLengthInst;
import fr.jamgotchian.abcd.core.tac.model.AssignConstInst;
import fr.jamgotchian.abcd.core.tac.model.AssignVarInst;
import fr.jamgotchian.abcd.core.tac.model.BinaryInst;
import fr.jamgotchian.abcd.core.tac.model.CallMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CastInst;
import fr.jamgotchian.abcd.core.tac.model.ChoiceInst;
import fr.jamgotchian.abcd.core.tac.model.ConditionalInst;
import fr.jamgotchian.abcd.core.tac.model.GetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.GetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.GetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.GotoInst;
import fr.jamgotchian.abcd.core.tac.model.InstanceOfInst;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
import fr.jamgotchian.abcd.core.tac.model.LabelInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorEnterInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorExitInst;
import fr.jamgotchian.abcd.core.tac.model.NewArrayInst;
import fr.jamgotchian.abcd.core.tac.model.NewObjectInst;
import fr.jamgotchian.abcd.core.tac.model.PhiInst;
import fr.jamgotchian.abcd.core.tac.model.ReturnInst;
import fr.jamgotchian.abcd.core.tac.model.SetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.SetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.SetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.SwitchInst;
import fr.jamgotchian.abcd.core.tac.model.TACInstSeq;
import fr.jamgotchian.abcd.core.tac.model.TACInstVisitor;
import fr.jamgotchian.abcd.core.tac.model.ThrowInst;
import fr.jamgotchian.abcd.core.tac.model.UnaryInst;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class EmptyTACInstVisitor<R, A> implements TACInstVisitor<R, A> {

    public R visit(TACInstSeq seq, A arg) {
        return null;
    }

    public R visit(ArrayLengthInst inst, A arg) {
        return null;
    }

    public R visit(AssignConstInst inst, A arg) {
        return null;
    }

    public R visit(AssignVarInst inst, A arg) {
        return null;
    }

    public R visit(BinaryInst inst, A arg) {
        return null;
    }

    public R visit(CallMethodInst inst, A arg) {
        return null;
    }

    public R visit(CallStaticMethodInst inst, A arg) {
        return null;
    }

    public R visit(CastInst inst, A arg) {
        return null;
    }

    public R visit(ConditionalInst inst, A arg) {
        return null;
    }

    public R visit(GetArrayInst inst, A arg) {
        return null;
    }

    public R visit(SetArrayInst inst, A arg) {
        return null;
    }

    public R visit(GetFieldInst inst, A arg) {
        return null;
    }

    public R visit(SetFieldInst inst, A arg) {
        return null;
    }

    public R visit(GotoInst inst, A arg) {
        return null;
    }

    public R visit(JumpIfInst inst, A arg) {
        return null;
    }

    public R visit(LabelInst inst, A arg) {
        return null;
    }

    public R visit(InstanceOfInst inst, A arg) {
        return null;
    }

    public R visit(MonitorEnterInst inst, A arg) {
        return null;
    }

    public R visit(MonitorExitInst inst, A arg) {
        return null;
    }

    public R visit(NewArrayInst inst, A arg) {
        return null;
    }

    public R visit(NewObjectInst inst, A arg) {
        return null;
    }

    public R visit(ReturnInst inst, A arg) {
        return null;
    }

    public R visit(SwitchInst inst, A arg) {
        return null;
    }

    public R visit(ThrowInst inst, A arg) {
        return null;
    }

    public R visit(UnaryInst inst, A arg) {
        return null;
    }

    public R visit(ChoiceInst inst, A arg) {
        return null;
    }

    public R visit(PhiInst inst, A arg) {
        return null;
    }

    public R visit(GetStaticFieldInst inst, A arg) {
        return null;
    }

    public R visit(SetStaticFieldInst inst, A arg) {
        return null;
    }

}
