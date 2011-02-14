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

package fr.jamgotchian.abcd.core.controlflow;

import org.objectweb.asm.tree.FieldInsnNode;
import org.objectweb.asm.tree.IincInsnNode;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.IntInsnNode;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LdcInsnNode;
import org.objectweb.asm.tree.LookupSwitchInsnNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MultiANewArrayInsnNode;
import org.objectweb.asm.tree.TableSwitchInsnNode;
import org.objectweb.asm.tree.TypeInsnNode;
import org.objectweb.asm.tree.VarInsnNode;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface BasicBlockVisitor {

    void before(BasicBlock block);
    
    void visitFieldInsn(BasicBlock block, int index, FieldInsnNode node);

    void visitIincInsn(BasicBlock block, int index, IincInsnNode node);

    void visitInsn(BasicBlock block, int index, InsnNode node);

    void visitIntInsn(BasicBlock block, int index, IntInsnNode node);

    void visitJumpInsn(BasicBlock block, int index, JumpInsnNode node);

    void visitLabel(BasicBlock block, int index, LabelNode node);

    void visitLdcInsn(BasicBlock block, int index, LdcInsnNode node);

    void visitLookupSwitchInsn(BasicBlock block, int index, LookupSwitchInsnNode node);

    void visitMethodInsn(BasicBlock block, int index, MethodInsnNode node);

    void visitMultiANewArrayInsn(BasicBlock block, int index, MultiANewArrayInsnNode node);

    void visitTableSwitchInsn(BasicBlock block, int index, TableSwitchInsnNode node);

    void visitTypeInsnInsn(BasicBlock block, int index, TypeInsnNode node);

    void visitVarInsn(BasicBlock block, int index, VarInsnNode node);

    void after(BasicBlock block);

}
