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

import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.BasicBlockType;
import fr.jamgotchian.abcd.core.code.CodeWriter;
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
import static org.objectweb.asm.util.AbstractVisitor.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BytecodeWriter extends BytecodeRangeVisitor {

    protected final CodeWriter writer;

    public BytecodeWriter(CodeWriter writer) {
        this.writer = writer;
    }

    public void before(BasicBlock bb) {
        writer.before(null);
        if (bb.getType() == BasicBlockType.ENTRY
                || bb.getType() == BasicBlockType.EXIT) {
            writer.write(bb.getType());
        }
    }

    private void writeEol(BasicBlock bb, int index) {
        if (index < bb.getRange().getLast()) {
            writer.newLine();
        }
    }

    public void writeFieldOrMethodInstn(BasicBlock bb, int index, int opcode, String scope, String fieldOrMethodName) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[opcode].toLowerCase());
        writer.writeSpace();
        writer.writeLt();
        writer.write(scope);
        writer.write(".");
        writer.write(fieldOrMethodName);
        writer.writeGt();
        writeEol(bb, index);
    }

    public void visitFieldInsn(BasicBlock bb, int index, FieldInsnNode node) {
        writeFieldOrMethodInstn(bb, index, node.getOpcode(), node.owner, node.name);
    }

    public void visitIincInsn(BasicBlock bb, int index, IincInsnNode node) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(Integer.toString(node.var));
        writer.writeSpace();
        writer.write(Integer.toString(node.incr));
        writeEol(bb, index);
    }

    public void visitInsn(BasicBlock bb, int index, InsnNode node) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writeEol(bb, index);
    }

    public void visitIntInsn(BasicBlock bb, int index, IntInsnNode node) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(Integer.toString(node.operand));
        writeEol(bb, index);
    }

    public void visitJumpInsn(BasicBlock bb, int index, JumpInsnNode node, LabelManager labelManager) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.writeLabel(labelManager.getLabel(node.label));
        writeEol(bb, index);
    }

    public void visitLabel(BasicBlock bb, int index, LabelNode node, LabelManager labelManager) {
        writer.writeIndex(index);
        writer.writeLabel(labelManager.getLabel(node));
        writeEol(bb, index);
    }

    public void visitLdcInsn(BasicBlock bb, int index, LdcInsnNode node) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(node.cst.toString());
        writeEol(bb, index);
    }

    public void visitLookupSwitchInsn(BasicBlock bb, int index, LookupSwitchInsnNode node, LabelManager labelManager) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(Integer.toString(node.keys.size()));
        writer.newLine();
        writer.incrIndent();
        for (int i = 0; i < node.keys.size(); i++) {
            writer.writeIndex(-1); // just for alignment
            writer.write(Integer.toString((Integer) node.keys.get(i)));
            writer.write(':');
            writer.writeSpace();
            writer.writeLabel(labelManager.getLabel((LabelNode) node.labels.get(i)));
            writer.newLine();
        }
        writer.decrIndent();
        writer.writeIndex(-1); // just for alignment
        writer.incrIndent();
        writer.writeKeyword("default:");
        writer.writeSpace();
        writer.writeLabel(labelManager.getLabel(node.dflt));
        writer.decrIndent();
        writeEol(bb, index);
    }

    public void visitMethodInsn(BasicBlock bb, int index, MethodInsnNode node) {
        writeFieldOrMethodInstn(bb, index, node.getOpcode(), node.owner, node.name);
    }

    public void visitMultiANewArrayInsn(BasicBlock bb, int index, MultiANewArrayInsnNode node) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(node.desc);
        writer.writeSpace();
        writer.write(Integer.toString(node.dims));
        writeEol(bb, index);
    }

    public void visitTableSwitchInsn(BasicBlock bb, int index, TableSwitchInsnNode node, LabelManager labelManager) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(Integer.toString(node.min));
        writer.write(" to ");
        writer.write(Integer.toString(node.max));
        writer.newLine();
        writer.incrIndent();
        for (int i = node.min; i <= node.max; i++) {
            writer.writeIndex(-1); // just for alignment
            writer.write(Integer.toString(i));
            writer.write(':');
            writer.writeSpace();
            writer.writeLabel(labelManager.getLabel((LabelNode) node.labels.get(i-node.min)));
            writer.newLine();
        }
        writer.decrIndent();
        writer.writeIndex(-1); // just for alignment
        writer.incrIndent();
        writer.write("default:");
        writer.writeSpace();
        writer.writeLabel(labelManager.getLabel(node.dflt));
        writer.decrIndent();
        writeEol(bb, index);
    }

    public void visitTypeInsnInsn(BasicBlock bb, int index, TypeInsnNode node) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.writeLt();
        writer.write(node.desc);
        writer.writeGt();
        writeEol(bb, index);
    }

    public void visitVarInsn(BasicBlock bb, int index, VarInsnNode node) {
        writer.writeIndex(index);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(Integer.toString(node.var));
        writeEol(bb, index);
    }

    public void after(BasicBlock bb) {
        writer.after(null);
    }
}
