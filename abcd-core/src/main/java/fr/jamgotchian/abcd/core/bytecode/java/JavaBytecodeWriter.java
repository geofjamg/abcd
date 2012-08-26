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

package fr.jamgotchian.abcd.core.bytecode.java;

import fr.jamgotchian.abcd.core.code.DOTHTMLLikeCodeWriter;
import fr.jamgotchian.abcd.core.code.HTMLCodeWriter;
import fr.jamgotchian.abcd.core.code.TextCodeWriter;
import fr.jamgotchian.abcd.core.ir.BasicBlockImpl;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.BasicBlockType;
import fr.jamgotchian.abcd.core.code.CodeWriter;
import java.io.StringWriter;
import org.objectweb.asm.tree.FieldInsnNode;
import org.objectweb.asm.tree.IincInsnNode;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.IntInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LdcInsnNode;
import org.objectweb.asm.tree.LookupSwitchInsnNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MultiANewArrayInsnNode;
import org.objectweb.asm.tree.TableSwitchInsnNode;
import org.objectweb.asm.tree.TypeInsnNode;
import org.objectweb.asm.tree.VarInsnNode;
import static org.objectweb.asm.util.Printer.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaBytecodeWriter extends JavaBytecodeVisitor {

    public static String toText(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new JavaBytecodeWriter(new TextCodeWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toText(InsnList instructions, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        BasicBlock bb = BasicBlockImpl.createRange(0, instructions.size()-1, null);
        new JavaBytecodeWriter(new TextCodeWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toHTML(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new JavaBytecodeWriter(new HTMLCodeWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toHTML(InsnList instructions, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        BasicBlock bb = BasicBlockImpl.createRange(0, instructions.size()-1, null);
        new JavaBytecodeWriter(new HTMLCodeWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toDOTHTMLLike(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new JavaBytecodeWriter(new DOTHTMLLikeCodeWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    protected final CodeWriter writer;

    public JavaBytecodeWriter(CodeWriter writer) {
        this.writer = writer;
    }

    @Override
    public void before(BasicBlock bb) {
        writer.before(null);
        if (bb.getType() == BasicBlockType.ENTRY
                || bb.getType() == BasicBlockType.EXIT) {
            writer.write(bb.getType());
        }
    }

    private void writeEol(BasicBlock bb, int position) {
        if (position < bb.getRange().getLast()) {
            writer.newLine();
        }
    }

    public void writeFieldOrMethodInstn(BasicBlock bb, int position, int opcode, String scope, String fieldOrMethodName) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[opcode].toLowerCase());
        writer.writeSpace();
        writer.writeLt();
        writer.write(scope);
        writer.write(".");
        writer.write(writer.removeSpecialCharacters(fieldOrMethodName));
        writer.writeGt();
        writeEol(bb, position);
    }

    @Override
    public void visitFieldInsn(BasicBlock bb, int position, FieldInsnNode node) {
        writeFieldOrMethodInstn(bb, position, node.getOpcode(), node.owner, node.name);
    }

    @Override
    public void visitIincInsn(BasicBlock bb, int position, IincInsnNode node) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(Integer.toString(node.var));
        writer.writeSpace();
        writer.write(Integer.toString(node.incr));
        writeEol(bb, position);
    }

    @Override
    public void visitInsn(BasicBlock bb, int position, InsnNode node) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writeEol(bb, position);
    }

    @Override
    public void visitIntInsn(BasicBlock bb, int position, IntInsnNode node) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(Integer.toString(node.operand));
        writeEol(bb, position);
    }

    @Override
    public void visitJumpInsn(BasicBlock bb, int position, JumpInsnNode node, LabelManager labelManager) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.writeLabel(labelManager.getLabel(node.label));
        writeEol(bb, position);
    }

    @Override
    public void visitLabel(BasicBlock bb, int position, LabelNode node, LabelManager labelManager) {
        writer.writeIndex(position);
        writer.writeLabel(labelManager.getLabel(node));
        writeEol(bb, position);
    }

    @Override
    public void visitLdcInsn(BasicBlock bb, int position, LdcInsnNode node) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(node.cst.toString());
        writeEol(bb, position);
    }

    @Override
    public void visitLookupSwitchInsn(BasicBlock bb, int position, LookupSwitchInsnNode node, LabelManager labelManager) {
        writer.writeIndex(position);
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
        writeEol(bb, position);
    }

    @Override
    public void visitMethodInsn(BasicBlock bb, int position, MethodInsnNode node) {
        writeFieldOrMethodInstn(bb, position, node.getOpcode(), node.owner, node.name);
    }

    @Override
    public void visitMultiANewArrayInsn(BasicBlock bb, int position, MultiANewArrayInsnNode node) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(node.desc);
        writer.writeSpace();
        writer.write(Integer.toString(node.dims));
        writeEol(bb, position);
    }

    @Override
    public void visitTableSwitchInsn(BasicBlock bb, int position, TableSwitchInsnNode node, LabelManager labelManager) {
        writer.writeIndex(position);
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
        writeEol(bb, position);
    }

    @Override
    public void visitTypeInsnInsn(BasicBlock bb, int position, TypeInsnNode node) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.writeLt();
        writer.write(node.desc);
        writer.writeGt();
        writeEol(bb, position);
    }

    @Override
    public void visitVarInsn(BasicBlock bb, int position, VarInsnNode node) {
        writer.writeIndex(position);
        writer.writeKeyword(OPCODES[node.getOpcode()].toLowerCase());
        writer.writeSpace();
        writer.write(Integer.toString(node.var));
        writeEol(bb, position);
    }

    @Override
    public void after(BasicBlock bb) {
        writer.after(null);
    }
}
