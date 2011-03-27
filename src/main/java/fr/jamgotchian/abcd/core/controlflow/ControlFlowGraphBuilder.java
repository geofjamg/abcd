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
import fr.jamgotchian.abcd.core.util.ASMUtil;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LookupSwitchInsnNode;
import org.objectweb.asm.tree.TableSwitchInsnNode;
import org.objectweb.asm.tree.TryCatchBlockNode;
import org.objectweb.asm.tree.MethodNode;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ControlFlowGraphBuilder {

    private static final Logger logger = Logger.getLogger(ControlFlowGraphBuilder.class.getName());
    
    static {
        logger.setLevel(Level.FINEST);
    }
    
    private static final String DEFAULT = "default";

    private Map<LabelNode, Integer> labelNodeIndex;

    private ControlFlowGraph graph;

    public ControlFlowGraph build(MethodNode mn, String methodName) {

        graph = new ControlFlowGraphImpl(methodName, mn.instructions);

        labelNodeIndex = ASMUtil.getLabelNodeIndexMap(mn.instructions);

        List<TryCatchBlock> tryCatchBlocks = new ArrayList<TryCatchBlock>();
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

            tryCatchBlocks.add(new TryCatchBlock(tryStart, tryEnd, catchStart, exceptionClassName));
        }

        analyseInstructions(mn.instructions);
        analyseTryCatchBlocks(tryCatchBlocks);
        
        return graph;
    }

    private void analyseTryCatchBlocks(List<TryCatchBlock> tryCatchBlocks) {
        for (TryCatchBlock tcb : tryCatchBlocks) {

            // split at tryStart and tryEnd
            BasicBlockSplit tryStartSplit = graph.splitBasicBlockAt(tcb.getTryStart());
            BasicBlockSplit tryEndSplit = graph.splitBasicBlockAt(tcb.getTryEnd());
            graph.addEdge(tryEndSplit.getBlockBefore(), tryEndSplit.getBlockAfter());

            // split at catchStart
            BasicBlockSplit catchStartSplit = graph.splitBasicBlockAt(tcb.getCatchStart());
            BasicBlock catchEntryBlock = catchStartSplit.getBlockAfter();
            graph.removeEdge(catchStartSplit.getBlockBefore(), catchEntryBlock);

            // link all blocks contained in the try range to the catch entry block
            for (BasicBlock block : graph.getBasicBlocksWithinRange(tcb.getTryStart(), tcb.getTryEnd() - 1)) {
                graph.addEdge(block, catchEntryBlock, true).setValue(tcb.getExceptionClassName());
            }
        }
    }

    private void analyseJumpNode(int currentInstIdx, JumpInsnNode jumpNode) {
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

                assert labelInstIdx != currentInstIdx;

                if (labelInstIdx > currentInstIdx + 1) {
                    // find the "then" block containing the label instruction
                    BasicBlockSplit thenSplitResult = graph.splitBasicBlockAt(labelInstIdx);
                    BasicBlock thenBlock = thenSplitResult.getBlockAfter();

                    // split the current block to get the "else" block
                    // current => [current -> else]
                    BasicBlockSplit elseSplitResult = graph.splitBasicBlockAt(currentInstIdx + 1);
                    BasicBlock currentBlock = elseSplitResult.getBlockBefore();
                    BasicBlock elseBlock = elseSplitResult.getBlockAfter();

                    graph.addEdge(currentBlock, elseBlock).setValue(Boolean.FALSE);

                    // link the current block to the "then" block
                    graph.addEdge(currentBlock, thenBlock).setValue(Boolean.TRUE);

                    currentBlock.setType(BasicBlockType.JUMP_IF);

                    logger.log(Level.FINER, "  JumpIf : current={0}, true={1}, false={2}",
                                             new Object[]{currentBlock, thenBlock, elseBlock});
                } else if (labelInstIdx == currentInstIdx + 1) {
                    // remove unnecessary instructions
                    BasicBlockSplit splitResult1 = graph.splitBasicBlockAt(currentInstIdx);
                    BasicBlockSplit splitResult2 = graph.splitBasicBlockAt(currentInstIdx+2);
                    graph.removeEdge(splitResult1.getBlockBefore(), splitResult1.getBlockAfter());
                    graph.removeEdge(splitResult2.getBlockBefore(), splitResult2.getBlockAfter());
                    graph.addEdge(splitResult1.getBlockBefore(), splitResult2.getBlockAfter());
                } else if (labelInstIdx < currentInstIdx) {
                    // find the "label" block containing the label instruction
                    BasicBlockSplit labelSplitResult = graph.splitBasicBlockAt(labelInstIdx);
                    BasicBlock labelBlock = labelSplitResult.getBlockAfter();

                    // current -> [current | remaining]
                    BasicBlockSplit jumpIfSplitResult = graph.splitBasicBlockAt(currentInstIdx+1);
                    BasicBlock currentBlock = jumpIfSplitResult.getBlockBefore();
                    BasicBlock exitBlock = jumpIfSplitResult.getBlockAfter();
                    graph.addEdge(currentBlock, exitBlock).setValue(Boolean.FALSE);

                    // link the current block to the "label" block
                    graph.addEdge(currentBlock, labelBlock).setValue(Boolean.TRUE);

                    currentBlock.setType(BasicBlockType.JUMP_IF);

                    logger.log(Level.FINER, "  JumpIf : current={0}, true={1}, false={2}",
                                             new Object[]{currentBlock, labelBlock, exitBlock});
                }
                break;
            }

            case GOTO: {
                LabelNode labelNode = jumpNode.label;
                int labelInstIdx = labelNodeIndex.get(labelNode);

                if (labelInstIdx > currentInstIdx + 1 || labelInstIdx < currentInstIdx) {
                    // find the "label" block containing the label instruction
                    BasicBlockSplit labelSplitResult = graph.splitBasicBlockAt(labelInstIdx);
                    BasicBlock labelBlock = labelSplitResult.getBlockAfter();

                    // current -> [current | remaining]
                    BasicBlockSplit gotoSplitResult = graph.splitBasicBlockAt(currentInstIdx+1);
                    BasicBlock currentBlock = gotoSplitResult.getBlockBefore();
                    BasicBlock remainingBlock = gotoSplitResult.getBlockAfter();
                    if (remainingBlock != null) {
                        graph.removeEdge(currentBlock, remainingBlock);
                    }

                    // link the current block to the "label" block
                    graph.addEdge(currentBlock, labelBlock);

                    currentBlock.setType(BasicBlockType.GOTO);

                    logger.log(Level.FINER, "  Goto : current={0}, label={1}",
                                             new Object[]{currentBlock, labelBlock});
                }
                break;
            }

            case JSR:
                throw new ABCDException("TODO : support JSR instruction");

            default:
                throw new ABCDException("Jump instruction unknown " + jumpNode.getOpcode());
        }
    }

    private void analyseSwitchNode(int currentInstIdx, List<LabelNode> labels, List<Object> keys) {
        BasicBlockSplit switchSplitResult = graph.splitBasicBlockAt(currentInstIdx+1);
        BasicBlock switchBlock = switchSplitResult.getBlockBefore();
        BasicBlock remainingBlock = switchSplitResult.getBlockAfter();

        graph.removeEdge(switchBlock, remainingBlock);

        List<BasicBlock> caseBlocks = new ArrayList<BasicBlock>();

        for (int i = 0; i < labels.size(); i++) {
            LabelNode label = labels.get(i);
            Object key = keys.get(i);
            int caseInstnIdx = labelNodeIndex.get(label);
            BasicBlockSplit caseSplitResult = graph.splitBasicBlockAt(caseInstnIdx);
            BasicBlock caseBlock = caseSplitResult.getBlockAfter();
            graph.addEdge(switchBlock, caseBlock).setValue(key);
            caseBlocks.add(caseBlock);
        }

        // necessary to tag the switch block after all case have been splitted
        switchBlock.setType(BasicBlockType.SWITCH);

        logger.log(Level.FINER, "  Switch : switch={0}, cases={1}",
                new Object[]{switchBlock, caseBlocks});
    }

    public void analyseInstructions(InsnList instructions) {

        for (int currentInstIdx = 0; currentInstIdx < instructions.size(); currentInstIdx++) {

            AbstractInsnNode node = instructions.get(currentInstIdx);

            switch (node.getType()) {
                case AbstractInsnNode.JUMP_INSN: {
                    JumpInsnNode jumpNode = (JumpInsnNode) node;

                    analyseJumpNode(currentInstIdx, jumpNode);
                    break;
                }

                case AbstractInsnNode.LOOKUPSWITCH_INSN: {
                    LookupSwitchInsnNode switchNode = (LookupSwitchInsnNode) node;

                    List<LabelNode> labels = new ArrayList<LabelNode>(switchNode.labels.size()+1);
                    labels.addAll(switchNode.labels);
                    labels.add(switchNode.dflt);
                    List<Object> keys = new ArrayList<Object>(switchNode.labels.size()+1);
                    keys.addAll(switchNode.keys);
                    keys.add(DEFAULT);

                    analyseSwitchNode(currentInstIdx, labels, keys);
                    break;
                }

                case AbstractInsnNode.TABLESWITCH_INSN: {
                    TableSwitchInsnNode switchNode = (TableSwitchInsnNode) node;

                    List<LabelNode> labels = new ArrayList<LabelNode>(switchNode.labels.size()+1);
                    labels.addAll(switchNode.labels);
                    labels.add(switchNode.dflt);
                    List<Object> keys = new ArrayList<Object>(switchNode.labels.size()+1);
                    for (int key = switchNode.min; key <= switchNode.max; key++) {
                        keys.add(key);
                    }
                    keys.add(DEFAULT);

                    analyseSwitchNode(currentInstIdx, labels, keys);
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
                            BasicBlockSplit returnSplitResult = graph.splitBasicBlockAt(currentInstIdx + 1);
                            BasicBlock returnBlock = returnSplitResult.getBlockBefore();
                            BasicBlock remainingBlock = returnSplitResult.getBlockAfter();
                            if (remainingBlock != null) {
                                graph.removeEdge(returnBlock, remainingBlock);
                            }
                            graph.addEdge(returnBlock, graph.getExitBlock());

                            returnBlock.setType(BasicBlockType.RETURN);

                            logger.log(Level.FINER, "  Return : current={0}", returnBlock);
                            break;
                    }
                    break;
                }
            }
        }
    }
}
