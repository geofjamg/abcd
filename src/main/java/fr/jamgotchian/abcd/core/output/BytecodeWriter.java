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

package fr.jamgotchian.abcd.core.output;

import fr.jamgotchian.abcd.core.controlflow.BasicBlockVisitor;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
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
public class BytecodeWriter implements BasicBlockVisitor {

    protected final InstnWriter writer;

    public BytecodeWriter(InstnWriter writer) {
        this.writer = writer;
    }

    public void before(BasicBlock block) {
        try {
            writer.begin();
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }
    
    public void visitFieldInsn(BasicBlock block, int index, FieldInsnNode node) {
        try {
            writer.writeFieldOrMethodInstn(index, node.getOpcode(), node.owner, node.name);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitIincInsn(BasicBlock block, int index, IincInsnNode node) {
        try {
            writer.writeIIncInstn(index, node.getOpcode(), node.var, node.incr);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitInsn(BasicBlock block, int index, InsnNode node) {
        try {
            writer.writeInstn(index, node.getOpcode());
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitIntInsn(BasicBlock block, int index, IntInsnNode node) {
        try {
            writer.writeIntInstn(index, node.getOpcode(), node.operand);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitJumpInsn(BasicBlock block, int index, JumpInsnNode node) {
        try {
            writer.writerJumpInstn(index, node.getOpcode(), block.getGraph().getLabelManager().getLabel(node.label).getId());
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitLabel(BasicBlock block, int index, LabelNode node) {
        try {
            writer.writeLabelInstn(index, block.getGraph().getLabelManager().getLabel(node).getId());
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitLdcInsn(BasicBlock block, int index, LdcInsnNode node) {
        try {
            writer.writeLdcInstn(index, node.getOpcode(), node.cst);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitLookupSwitchInsn(BasicBlock block, int index, LookupSwitchInsnNode node) {
        try {
            List<Integer> labelsIndex = new ArrayList<Integer>(node.labels.size());
            for (int i = 0; i < node.labels.size(); i++) {
                LabelNode labelNode = (LabelNode) node.labels.get(i);
                labelsIndex.add(block.getGraph().getLabelManager().getLabel(labelNode).getId());
            }
            writer.writeLookupSwitchInstn(index, node.getOpcode(), node.keys,
                                          block.getGraph().getLabelManager().getLabel(node.dflt).getId(), labelsIndex);
            
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitMethodInsn(BasicBlock block, int index, MethodInsnNode node) {
        try {
            writer.writeFieldOrMethodInstn(index, node.getOpcode(), node.owner, node.name);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitMultiANewArrayInsn(BasicBlock block, int index, MultiANewArrayInsnNode node) {
        try {
            writer.writeMultiANewArrayInstn(index, node.getOpcode());
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitTableSwitchInsn(BasicBlock block, int index, TableSwitchInsnNode node) {
        try {
            List<Integer> labelsIndex = new ArrayList<Integer>(node.labels.size());
            for (int i = 0; i < node.labels.size(); i++) {
                LabelNode labelNode = (LabelNode) node.labels.get(i);
                labelsIndex.add(block.getGraph().getLabelManager().getLabel(labelNode).getId());
            }
            writer.writeTableSwitchInstn(index, node.getOpcode(), node.min, node.max,
                                         block.getGraph().getLabelManager().getLabel(node.dflt).getId(), labelsIndex);            
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitTypeInsnInsn(BasicBlock block, int index, TypeInsnNode node) {
        try {
            writer.writeTypeInstn(index, node.getOpcode(), node.desc);            
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void visitVarInsn(BasicBlock block, int index, VarInsnNode node) {
        try {
            writer.writeVarInstn(index, node.getOpcode(), node.var);
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

    public void after(BasicBlock block) {
        try {
            writer.end();
        } catch(IOException exc) {
            throw new ABCDException(exc);
        }
    }

}
