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

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.CaseValues;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraphBuilder;
import fr.jamgotchian.abcd.core.ir.ExceptionTable;
import fr.jamgotchian.abcd.core.ir.LocalVariableTable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.tree.LocalVariableNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.TryCatchBlockNode;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LookupSwitchInsnNode;
import org.objectweb.asm.tree.TableSwitchInsnNode;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaBytecodeControlFlowGraphBuilder extends ControlFlowGraphBuilder {

    private final MethodNode mn;

    private final LabelManager labelManager;

    private final Map<LabelNode, Integer> labelNodeIndex;

    public JavaBytecodeControlFlowGraphBuilder(String methodName, MethodNode mn,
                                               LabelManager labelManager) {
        super(methodName);
        this.mn = mn;
        this.labelManager = labelManager;
        labelNodeIndex = new HashMap<LabelNode, Integer>();
        for (int i = 0; i < mn.instructions.size(); i++) {
            AbstractInsnNode node = mn.instructions.get(i);
            if (node.getType() == AbstractInsnNode.LABEL) {
                labelNodeIndex.put((LabelNode) node, i);
            }
        }
    }

    @Override
    protected int getInstructionCount() {
        return mn.instructions.size();
    }

    @Override
    protected GraphvizRenderer<BasicBlock> getGraphizRenderer() {
        return new JavaBytecodeGraphvizRenderer(mn.instructions, labelManager);
    }

    @Override
    protected ExceptionTable getExceptionTable() {
        ExceptionTable table = new ExceptionTable();
        for (int i = 0; i < mn.tryCatchBlocks.size(); i++) {
            TryCatchBlockNode node = (TryCatchBlockNode) mn.tryCatchBlocks.get(i);

            int catchStart = labelNodeIndex.get(node.handler);
            int tryStart = labelNodeIndex.get(node.start);
            int tryEnd = labelNodeIndex.get(node.end);
            String exceptionClassName = node.type;
            if (exceptionClassName != null) {
                exceptionClassName = exceptionClassName.replace('/', '.');
            }

            if (tryStart >= catchStart) { // ???
                continue;
            }

            table.addEntry(tryStart, tryEnd, catchStart, exceptionClassName);
        }
        return table;
    }

    @Override
    protected LocalVariableTable getLocalVariableTable() {
        LocalVariableTable table = new LocalVariableTable();
        for (int i = 0; i < mn.localVariables.size(); i++) {
            LocalVariableNode node = (LocalVariableNode) mn.localVariables.get(i);
            table.addEntry(node.index,
                      labelNodeIndex.get(node.start),
                      labelNodeIndex.get(node.end),
                      node.name,
                      node.desc);
        }
        return table;
    }

    @Override
    protected void analyseInstructions() {
        for (int currentInstIdx = 0; currentInstIdx < mn.instructions.size();
                currentInstIdx++) {
            AbstractInsnNode node = mn.instructions.get(currentInstIdx);

            switch (node.getType()) {
                case AbstractInsnNode.JUMP_INSN: {
                    JumpInsnNode jumpNode = (JumpInsnNode) node;

                    switch (jumpNode.getOpcode()) {
                        case IFEQ:
                        case IFNE:
                        case IFLT:
                        case IFGE:
                        case IFGT:
                        case IFLE:
                        case IF_ICMPEQ:
                        case IF_ICMPNE:
                        case IF_ICMPLT:
                        case IF_ICMPGE:
                        case IF_ICMPGT:
                        case IF_ICMPLE:
                        case IF_ACMPEQ:
                        case IF_ACMPNE:
                        case IFNULL:
                        case IFNONNULL: {
                            LabelNode labelNode = jumpNode.label;
                            int labelInstIdx = labelNodeIndex.get(labelNode);
                            analyseJumpInst(currentInstIdx, labelInstIdx);
                            break;
                        }

                        case GOTO: {
                            LabelNode labelNode = jumpNode.label;
                            int labelInstIdx = labelNodeIndex.get(labelNode);
                            analyseGotoInst(currentInstIdx, labelInstIdx);
                            break;
                        }

                        case JSR:
                            throw new ABCDException("TODO : support JSR instruction");

                        default:
                            throw new ABCDException("Jump instruction unknown " + jumpNode.getOpcode());
                    }
                    break;
                }

                case AbstractInsnNode.LOOKUPSWITCH_INSN: {
                    LookupSwitchInsnNode switchNode = (LookupSwitchInsnNode) node;

                    List<Integer> caseInstnIdxs = new ArrayList<Integer>(switchNode.labels.size()+1);
                    for (Object label : switchNode.labels) {
                        caseInstnIdxs.add(labelNodeIndex.get(((LabelNode) label)));
                    }
                    caseInstnIdxs.add(labelNodeIndex.get(switchNode.dflt));

                    List<CaseValues> values = new ArrayList<CaseValues>(switchNode.labels.size()+1);
                    for (Object key : switchNode.keys) {
                        values.add(CaseValues.newValue(key.toString()));
                    }
                    values.add(CaseValues.newDefaultValue());

                    analyseSwitchInst(currentInstIdx, caseInstnIdxs, values);
                    break;
                }

                case AbstractInsnNode.TABLESWITCH_INSN: {
                    TableSwitchInsnNode switchNode = (TableSwitchInsnNode) node;

                    List<Integer> caseInstnIdxs = new ArrayList<Integer>(switchNode.labels.size()+1);
                    for (Object label : switchNode.labels) {
                        caseInstnIdxs.add(labelNodeIndex.get(((LabelNode) label)));
                    }
                    caseInstnIdxs.add(labelNodeIndex.get(switchNode.dflt));

                    List<CaseValues> values = new ArrayList<CaseValues>(switchNode.labels.size()+1);
                    for (int key = switchNode.min; key <= switchNode.max; key++) {
                        values.add(CaseValues.newValue(Integer.toString(key)));
                    }
                    values.add(CaseValues.newDefaultValue());

                    analyseSwitchInst(currentInstIdx, caseInstnIdxs, values);
                    break;
                }

                case AbstractInsnNode.INSN: {
                    InsnNode insnNode = (InsnNode) node;
                    switch (insnNode.getOpcode()) {
                        case IRETURN:
                        case LRETURN:
                        case FRETURN:
                        case DRETURN:
                        case ARETURN:
                        case RETURN:
                            analyseReturnInst(currentInstIdx);
                            break;

                        case ATHROW:
                            analyseThrowInst(currentInstIdx);
                            break;
                    }
                    break;
                }
            }
        }
    }
}
