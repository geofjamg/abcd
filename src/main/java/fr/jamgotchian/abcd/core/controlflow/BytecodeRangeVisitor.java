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

import org.objectweb.asm.tree.InsnList;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.common.LabelManager;
import fr.jamgotchian.abcd.core.util.Range;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.FieldInsnNode;
import org.objectweb.asm.tree.FrameNode;
import org.objectweb.asm.tree.IincInsnNode;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.IntInsnNode;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LdcInsnNode;
import org.objectweb.asm.tree.LineNumberNode;
import org.objectweb.asm.tree.LookupSwitchInsnNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MultiANewArrayInsnNode;
import org.objectweb.asm.tree.TableSwitchInsnNode;
import org.objectweb.asm.tree.TypeInsnNode;
import org.objectweb.asm.tree.VarInsnNode;
import static org.objectweb.asm.tree.AbstractInsnNode.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class BytecodeRangeVisitor {

    public abstract void before(BasicBlock bb);

    public abstract void visitFieldInsn(BasicBlock bb, int position, FieldInsnNode node);

    public abstract void visitIincInsn(BasicBlock bb, int position, IincInsnNode node);

    public abstract void visitInsn(BasicBlock bb, int position, InsnNode node);

    public abstract void visitIntInsn(BasicBlock bb, int position, IntInsnNode node);

    public abstract void visitJumpInsn(BasicBlock bb, int position, JumpInsnNode node, LabelManager labelManager);

    public abstract void visitLabel(BasicBlock bb, int position, LabelNode node, LabelManager labelManager);

    public abstract void visitLdcInsn(BasicBlock bb, int position, LdcInsnNode node);

    public abstract void visitLookupSwitchInsn(BasicBlock bb, int position, LookupSwitchInsnNode node, LabelManager labelManager);

    public abstract void visitMethodInsn(BasicBlock bb, int position, MethodInsnNode node);

    public abstract void visitMultiANewArrayInsn(BasicBlock bb, int position, MultiANewArrayInsnNode node);

    public abstract void visitTableSwitchInsn(BasicBlock bb, int position, TableSwitchInsnNode node, LabelManager labelManager);

    public abstract void visitTypeInsnInsn(BasicBlock bb, int position, TypeInsnNode node);

    public abstract void visitVarInsn(BasicBlock bb, int position, VarInsnNode node);

    public abstract void after(BasicBlock bb);

    public void visit(InsnList instructions, BasicBlock bb, LabelManager labelManager) {

        before(bb);

        Range range = bb.getRange();

        if (range != null) {
            for (int position : range) {
                AbstractInsnNode abstractNode = instructions.get(position);

                switch (abstractNode.getType()) {
                    case FIELD_INSN: {
                        FieldInsnNode node = (FieldInsnNode) abstractNode;
                        visitFieldInsn(bb, position, node);
                        break;
                    }

                    case FRAME: {
                        FrameNode node = (FrameNode) abstractNode;
                        break;
                    }

                    case IINC_INSN: {
                        IincInsnNode node = (IincInsnNode) abstractNode;
                        visitIincInsn(bb, position, node);
                        break;
                    }

                    case INSN: {
                        InsnNode node = (InsnNode) abstractNode;
                        visitInsn(bb, position, node);
                        break;
                    }

                    case INT_INSN: {
                        IntInsnNode node = (IntInsnNode) abstractNode;
                        visitIntInsn(bb, position, node);
                        break;
                    }

                    case JUMP_INSN: {
                        JumpInsnNode node = (JumpInsnNode) abstractNode;
                        visitJumpInsn(bb, position, node, labelManager);
                        break;
                    }

                    case LABEL: {
                        LabelNode node = (LabelNode) abstractNode;
                        visitLabel(bb, position, node, labelManager);
                        break;
                    }

                    case LDC_INSN: {
                        LdcInsnNode node = (LdcInsnNode) abstractNode;
                        visitLdcInsn(bb, position, node);
                        break;
                    }

                    case LINE: {
                        LineNumberNode node = (LineNumberNode) abstractNode;
                        break;
                    }

                    case LOOKUPSWITCH_INSN: {
                        LookupSwitchInsnNode node = (LookupSwitchInsnNode) abstractNode;
                        visitLookupSwitchInsn(bb, position, node, labelManager);
                        break;
                    }

                    case METHOD_INSN: {
                        MethodInsnNode node = (MethodInsnNode) abstractNode;
                        visitMethodInsn(bb, position, node);
                        break;
                    }

                    case MULTIANEWARRAY_INSN: {
                        MultiANewArrayInsnNode node = (MultiANewArrayInsnNode) abstractNode;
                        visitMultiANewArrayInsn(bb, position, node);
                        break;
                    }

                    case TABLESWITCH_INSN: {
                        TableSwitchInsnNode node = (TableSwitchInsnNode) abstractNode;
                        visitTableSwitchInsn(bb, position, node, labelManager);
                        break;
                    }

                    case TYPE_INSN: {
                        TypeInsnNode node = (TypeInsnNode) abstractNode;
                        visitTypeInsnInsn(bb, position, node);
                        break;
                    }

                    case VAR_INSN: {
                        VarInsnNode node = (VarInsnNode) abstractNode;
                        visitVarInsn(bb, position, node);
                        break;
                    }

                    default:
                        throw new ABCDException("Unknown type instruction : " + abstractNode.getType());
                }
            }
        }

        after(bb);
    }
}
