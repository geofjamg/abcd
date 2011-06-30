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

    void visitFieldInsn(BasicBlock block, int position, FieldInsnNode node);

    void visitIincInsn(BasicBlock block, int position, IincInsnNode node);

    void visitInsn(BasicBlock block, int position, InsnNode node);

    void visitIntInsn(BasicBlock block, int position, IntInsnNode node);

    void visitJumpInsn(BasicBlock block, int position, JumpInsnNode node);

    void visitLabel(BasicBlock block, int position, LabelNode node);

    void visitLdcInsn(BasicBlock block, int position, LdcInsnNode node);

    void visitLookupSwitchInsn(BasicBlock block, int position, LookupSwitchInsnNode node);

    void visitMethodInsn(BasicBlock block, int position, MethodInsnNode node);

    void visitMultiANewArrayInsn(BasicBlock block, int position, MultiANewArrayInsnNode node);

    void visitTableSwitchInsn(BasicBlock block, int position, TableSwitchInsnNode node);

    void visitTypeInsnInsn(BasicBlock block, int position, TypeInsnNode node);

    void visitVarInsn(BasicBlock block, int position, VarInsnNode node);

    void after(BasicBlock block);

}
