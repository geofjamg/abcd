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

package fr.jamgotchian.abcd.core.ir.bytecode;

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.BasicBlockType;
import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
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
public class BytecodeWriter extends BytecodeRangeVisitor {

    protected final InstnWriter writer;

    public BytecodeWriter(InstnWriter writer) {
        this.writer = writer;
    }

    private void writeEol(BasicBlock bb, int index) throws IOException {
        if (index < bb.getRange().getLast()) {
            writer.writeEol();
        }
    }

    public void before(BasicBlock bb) {
        try {
            writer.begin();
            if (bb.getType() == BasicBlockType.ENTRY) {
                writer.write("ENTRY", Color.BLACK);
            } else if (bb.getType() == BasicBlockType.EXIT) {
                writer.write("EXIT", Color.BLACK);
            }
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitFieldInsn(BasicBlock bb, int index, FieldInsnNode node) {
        try {
            writer.writeFieldOrMethodInstn(index, node.getOpcode(), node.owner, node.name);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitIincInsn(BasicBlock bb, int index, IincInsnNode node) {
        try {
            writer.writeIIncInstn(index, node.getOpcode(), node.var, node.incr);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitInsn(BasicBlock bb, int index, InsnNode node) {
        try {
            writer.writeInstn(index, node.getOpcode());
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitIntInsn(BasicBlock bb, int index, IntInsnNode node) {
        try {
            writer.writeIntInstn(index, node.getOpcode(), node.operand);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitJumpInsn(BasicBlock bb, int index, JumpInsnNode node, LabelManager labelManager) {
        try {
            writer.writerJumpInstn(index, node.getOpcode(), labelManager.getLabel(node.label).getId());
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitLabel(BasicBlock bb, int index, LabelNode node, LabelManager labelManager) {
        try {
            writer.writeLabelInstn(index, labelManager.getLabel(node).getId());
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitLdcInsn(BasicBlock bb, int index, LdcInsnNode node) {
        try {
            writer.writeLdcInstn(index, node.getOpcode(), node.cst);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitLookupSwitchInsn(BasicBlock bb, int index, LookupSwitchInsnNode node, LabelManager labelManager) {
        try {
            List<Integer> labelsIndex = new ArrayList<Integer>(node.labels.size());
            for (int i = 0; i < node.labels.size(); i++) {
                LabelNode labelNode = (LabelNode) node.labels.get(i);
                labelsIndex.add(labelManager.getLabel(labelNode).getId());
            }
            writer.writeLookupSwitchInstn(index, node.getOpcode(), node.keys,
                                          labelManager.getLabel(node.dflt).getId(), labelsIndex);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitMethodInsn(BasicBlock bb, int index, MethodInsnNode node) {
        try {
            writer.writeFieldOrMethodInstn(index, node.getOpcode(), node.owner, node.name);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitMultiANewArrayInsn(BasicBlock bb, int index, MultiANewArrayInsnNode node) {
        try {
            writer.writeMultiANewArrayInstn(index, node.getOpcode(), node.desc, node.dims);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitTableSwitchInsn(BasicBlock bb, int index, TableSwitchInsnNode node, LabelManager labelManager) {
        try {
            List<Integer> labelsIndex = new ArrayList<Integer>(node.labels.size());
            for (int i = 0; i < node.labels.size(); i++) {
                LabelNode labelNode = (LabelNode) node.labels.get(i);
                labelsIndex.add(labelManager.getLabel(labelNode).getId());
            }
            writer.writeTableSwitchInstn(index, node.getOpcode(), node.min, node.max,
                                         labelManager.getLabel(node.dflt).getId(), labelsIndex);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitTypeInsnInsn(BasicBlock bb, int index, TypeInsnNode node) {
        try {
            writer.writeTypeInstn(index, node.getOpcode(), node.desc);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitVarInsn(BasicBlock bb, int index, VarInsnNode node) {
        try {
            writer.writeVarInstn(index, node.getOpcode(), node.var);
            writeEol(bb, index);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void after(BasicBlock bb) {
        try {
            writer.end();
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

}
