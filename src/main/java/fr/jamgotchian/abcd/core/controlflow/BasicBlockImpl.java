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

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.util.Range;
import fr.jamgotchian.abcd.core.util.RangeImpl;
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
class BasicBlockImpl implements BasicBlock {

    private final Range range;

    private BasicBlockType type;

    private int order;

    private int loopLevel;

    private final TACInstSeq instructions;

    private VariableStack inputStack;

    private VariableStack outputStack;

    private ControlFlowGraph graph;

    BasicBlockImpl(Range range, BasicBlockType type) {
        this.range = range;
        this.type = type;
        order = -1;
        loopLevel = 0;
        instructions = new TACInstSeq();
    }

    BasicBlockImpl(int firstInstn, int lastInstn, BasicBlockType type) {
        this(new RangeImpl(firstInstn, lastInstn), type);
    }

    BasicBlockImpl(BasicBlockType type) {
        this(null, type);
    }

    BasicBlockImpl() {
        this(null, null);
    }

    public Range getRange() {
        return range;
    }

    public void setGraph(ControlFlowGraph graph) {
        this.graph = graph;
    }

    public ControlFlowGraph getGraph() {
        return graph;
    }

    public BasicBlockType getType() {
        return type;
    }

    public void setType(BasicBlockType type) {
        this.type = type;
    }

    public void setOrder(int order) {
        this.order = order;
    }

    public int getOrder() {
        return order;
    }

    public int getLoopLevel() {
        return loopLevel;
    }

    public void setLoopLevel(int loopLevel) {
        this.loopLevel = loopLevel;
    }

    public TACInstSeq getInstructions() {
        return instructions;
    }

    public VariableStack getInputStack() {
        return inputStack;
    }

    public void setInputStack(VariableStack inputStack) {
        this.inputStack = inputStack;
    }

    public VariableStack getOutputStack() {
        return outputStack;
    }

    public void setOutputStack(VariableStack outputStack) {
        this.outputStack = outputStack;
    }

    public void visit(BasicBlockVisitor visitor) {

        visitor.before(this);

        if (range != null) {
            for (int position : range) {
                AbstractInsnNode abstractNode = graph.getInstructions().get(position);

                switch (abstractNode.getType()) {
                    case FIELD_INSN: {
                        FieldInsnNode node = (FieldInsnNode) abstractNode;
                        visitor.visitFieldInsn(this, position, node);
                        break;
                    }

                    case FRAME: {
                        FrameNode node = (FrameNode) abstractNode;
                        break;
                    }

                    case IINC_INSN: {
                        IincInsnNode node = (IincInsnNode) abstractNode;
                        visitor.visitIincInsn(this, position, node);
                        break;
                    }

                    case INSN: {
                        InsnNode node = (InsnNode) abstractNode;
                        visitor.visitInsn(this, position, node);
                        break;
                    }

                    case INT_INSN: {
                        IntInsnNode node = (IntInsnNode) abstractNode;
                        visitor.visitIntInsn(this, position, node);
                        break;
                    }

                    case JUMP_INSN: {
                        JumpInsnNode node = (JumpInsnNode) abstractNode;
                        visitor.visitJumpInsn(this, position, node);
                        break;
                    }

                    case LABEL: {
                        LabelNode node = (LabelNode) abstractNode;
                        visitor.visitLabel(this, position, node);
                        break;
                    }

                    case LDC_INSN: {
                        LdcInsnNode node = (LdcInsnNode) abstractNode;
                        visitor.visitLdcInsn(this, position, node);
                        break;
                    }

                    case LINE: {
                        LineNumberNode node = (LineNumberNode) abstractNode;
                        break;
                    }

                    case LOOKUPSWITCH_INSN: {
                        LookupSwitchInsnNode node = (LookupSwitchInsnNode) abstractNode;
                        visitor.visitLookupSwitchInsn(this, position, node);
                        break;
                    }

                    case METHOD_INSN: {
                        MethodInsnNode node = (MethodInsnNode) abstractNode;
                        visitor.visitMethodInsn(this, position, node);
                        break;
                    }

                    case MULTIANEWARRAY_INSN: {
                        MultiANewArrayInsnNode node = (MultiANewArrayInsnNode) abstractNode;
                        visitor.visitMultiANewArrayInsn(this, position, node);
                        break;
                    }

                    case TABLESWITCH_INSN: {
                        TableSwitchInsnNode node = (TableSwitchInsnNode) abstractNode;
                        visitor.visitTableSwitchInsn(this, position, node);
                        break;
                    }

                    case TYPE_INSN: {
                        TypeInsnNode node = (TypeInsnNode) abstractNode;
                        visitor.visitTypeInsnInsn(this, position, node);
                        break;
                    }

                    case VAR_INSN: {
                        VarInsnNode node = (VarInsnNode) abstractNode;
                        visitor.visitVarInsn(this, position, node);
                        break;
                    }

                    default:
                        throw new ABCDException("Unknown type instruction : " + abstractNode.getType());
                }
            }
        }

        visitor.after(this);
    }

    @Override
    public String toString() {
        if (type != null) {
            switch (type) {
                case ENTRY:
                case EXIT:
                    return type.name();
            }
        }
        return range == null ? "[]" : range.toString();
    }
}
