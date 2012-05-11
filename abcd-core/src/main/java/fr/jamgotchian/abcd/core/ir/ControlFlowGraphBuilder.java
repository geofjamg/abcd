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

package fr.jamgotchian.abcd.core.ir;

import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class ControlFlowGraphBuilder {

    private static final Logger LOGGER
            = Logger.getLogger(ControlFlowGraphBuilder.class.getName());

    private final String methodName;

    private ControlFlowGraph cfg;

    public ControlFlowGraphBuilder(String methodName) {
        this.methodName = methodName;
    }

    public ControlFlowGraph build() {
        ConsoleUtil.logTitledSeparator(LOGGER, Level.FINE, "Build CFG of {0}",
                '=', methodName);

        cfg = new ControlFlowGraph(methodName, getInstructionCount());

        ExceptionTable table = getExceptionTable();
        cfg.setExceptionTable(getExceptionTable());
        printExceptionTable(table);

        analyseInstructions();

        analyseExceptionTable(table);

        LocalVariableTable table2 = getLocalVariableTable();
        cfg.setLocalVariableTable(table2);
        printLocalVariableTable(table2);

        return cfg;
    }

    protected abstract void analyseInstructions();

    protected abstract int getInstructionCount();

    protected abstract GraphvizRenderer<BasicBlock> getGraphizRenderer();

    protected abstract ExceptionTable getExceptionTable();

    protected abstract LocalVariableTable getLocalVariableTable();

    protected void analyseExceptionTable(ExceptionTable table) {
        for (ExceptionTable.Entry entry : table.getEntries()) {
            // split at tryStart and tryEnd
            cfg.splitBasicBlockAt(entry.getTryStart());
            cfg.splitBasicBlockAt(entry.getTryEnd());

            // split at catchStart
            BasicBlockSplit catchStartSplit = cfg.splitBasicBlockAt(entry.getCatchStart());
            BasicBlock catchEntryBlock = catchStartSplit.getBlockAfter();
            ExceptionHandlerInfo info = new ExceptionHandlerInfo(entry.getExceptionClassName());
            catchEntryBlock.putProperty(BasicBlockPropertyName.EXCEPTION_HANDLER_ENTRY, info);
            if (entry.getExceptionClassName() == null) {
                catchEntryBlock.putProperty(BasicBlockPropertyName.FINALLY_ENTRY, null);
            }

            cfg.removeEdge(catchStartSplit.getBlockBefore(), catchEntryBlock);

            // link all blocks contained in the try range to the catch entry block
            for (BasicBlock block : cfg.getBasicBlocksWithinRange(entry.getTryStart(), entry.getTryEnd() - 1)) {
                cfg.addEdge(block, catchEntryBlock, true);
            }
        }
    }

    private void printExceptionTable(ExceptionTable table) {
        StringBuilder builder = new StringBuilder();
        table.print(builder);
        LOGGER.log(Level.FINER, "Exception table :\n{0}", builder.toString());
    }

    private void printLocalVariableTable(LocalVariableTable table) {
        StringBuilder builder = new StringBuilder();
        table.print(builder);
        LOGGER.log(Level.FINER, "Local variable table :\n{0}", builder.toString());
    }

    protected void analyseJumpInst(int currentInstIdx, int labelInstIdx) {
        assert labelInstIdx != currentInstIdx;
        if (labelInstIdx > currentInstIdx + 1) {
            // find the "then" block containing the label instruction
            BasicBlockSplit thenSplitResult = cfg.splitBasicBlockAt(labelInstIdx);
            BasicBlock thenBlock = thenSplitResult.getBlockAfter();

            // split the current block to get the "else" block
            // current => [current -> else]
            BasicBlockSplit elseSplitResult = cfg.splitBasicBlockAt(currentInstIdx + 1);
            BasicBlock currentBlock = elseSplitResult.getBlockBefore();
            BasicBlock elseBlock = elseSplitResult.getBlockAfter();

            cfg.addEdge(currentBlock, elseBlock).setValue(Boolean.FALSE);

            // link the current block to the "then" block
            cfg.addEdge(currentBlock, thenBlock).setValue(Boolean.TRUE);

            currentBlock.setType(BasicBlockType.JUMP_IF);

            LOGGER.log(Level.FINER, "  JumpIf : current={0}, true={1}, false={2}",
                                     new Object[]{currentBlock, thenBlock, elseBlock});
        } else if (labelInstIdx == currentInstIdx + 1) {
            // remove unnecessary jump
            //
            // n     jumpif Lx
            // n+1   Lx:
            //
            BasicBlockSplit splitResult1 = cfg.splitBasicBlockAt(currentInstIdx);
            BasicBlockSplit splitResult2 = cfg.splitBasicBlockAt(currentInstIdx+2);
            cfg.removeEdge(splitResult1.getBlockBefore(), splitResult1.getBlockAfter());
            cfg.removeEdge(splitResult2.getBlockBefore(), splitResult2.getBlockAfter());
            cfg.addEdge(splitResult1.getBlockBefore(), splitResult2.getBlockAfter());
        } else if (labelInstIdx < currentInstIdx) {
            // find the "label" block containing the label instruction
            BasicBlockSplit labelSplitResult = cfg.splitBasicBlockAt(labelInstIdx);
            BasicBlock labelBlock = labelSplitResult.getBlockAfter();

            // current -> [current | remaining]
            BasicBlockSplit jumpIfSplitResult = cfg.splitBasicBlockAt(currentInstIdx+1);
            BasicBlock currentBlock = jumpIfSplitResult.getBlockBefore();
            BasicBlock exitBlock = jumpIfSplitResult.getBlockAfter();
            cfg.addEdge(currentBlock, exitBlock).setValue(Boolean.FALSE);

            // link the current block to the "label" block
            cfg.addEdge(currentBlock, labelBlock).setValue(Boolean.TRUE);

            currentBlock.setType(BasicBlockType.JUMP_IF);

            LOGGER.log(Level.FINER, "  JumpIf : current={0}, true={1}, false={2}",
                                     new Object[]{currentBlock, labelBlock, exitBlock});
        }
    }

    protected void analyseGotoInst(int currentInstIdx, int labelInstIdx) {
        if (labelInstIdx > currentInstIdx + 1 || labelInstIdx < currentInstIdx) {
            // find the "label" block containing the label instruction
            BasicBlockSplit labelSplitResult = cfg.splitBasicBlockAt(labelInstIdx);
            BasicBlock labelBlock = labelSplitResult.getBlockAfter();

            // current -> [current | remaining]
            BasicBlockSplit gotoSplitResult = cfg.splitBasicBlockAt(currentInstIdx+1);
            BasicBlock currentBlock = gotoSplitResult.getBlockBefore();
            BasicBlock remainingBlock = gotoSplitResult.getBlockAfter();
            if (remainingBlock != null) {
                cfg.removeEdge(currentBlock, remainingBlock);
            }

            // link the current block to the "label" block
            cfg.addEdge(currentBlock, labelBlock);

            currentBlock.setType(BasicBlockType.GOTO);

            LOGGER.log(Level.FINER, "  Goto : current={0}, label={1}",
                                     new Object[]{currentBlock, labelBlock});
        }
    }

    protected void analyseReturnInst(int currentInstIdx) {
        BasicBlockSplit returnSplitResult = cfg.splitBasicBlockAt(currentInstIdx + 1);
        BasicBlock returnBlock = returnSplitResult.getBlockBefore();
        BasicBlock remainingBlock = returnSplitResult.getBlockAfter();
        if (remainingBlock != null) {
            cfg.removeEdge(returnBlock, remainingBlock);
        }
        cfg.addEdge(returnBlock, cfg.getExitBlock());

        returnBlock.setType(BasicBlockType.RETURN);

        LOGGER.log(Level.FINER, "  Return : current={0}", returnBlock);
    }

    protected void analyseThrowInst(int currentInstIdx) {
        BasicBlockSplit throwSplitResult = cfg.splitBasicBlockAt(currentInstIdx + 1);
        BasicBlock throwBlock = throwSplitResult.getBlockBefore();
        BasicBlock remainingBlock = throwSplitResult.getBlockAfter();
        if (remainingBlock != null) {
            cfg.removeEdge(throwBlock, remainingBlock);
        }

        LOGGER.log(Level.FINER, "  Throw : current={0}", throwBlock);
    }

    protected void analyseSwitchInst(int currentInstIdx, List<Integer> caseInstnIdxs, List<CaseValues> values) {
        BasicBlockSplit switchSplitResult = cfg.splitBasicBlockAt(currentInstIdx+1);
        BasicBlock switchBlock = switchSplitResult.getBlockBefore();
        BasicBlock remainingBlock = switchSplitResult.getBlockAfter();

        cfg.removeEdge(switchBlock, remainingBlock);

        // first split and then link to allow parallel edges (several value to
        // the same case basic block)
        Map<BasicBlock, CaseValues> caseBlocks = new LinkedHashMap<BasicBlock, CaseValues>();
        for (int i = 0; i < caseInstnIdxs.size(); i++) {
            int caseInstnIdx = caseInstnIdxs.get(i);
            CaseValues value = values.get(i);
            BasicBlockSplit caseSplitResult = cfg.splitBasicBlockAt(caseInstnIdx);
            BasicBlock caseBlock = caseSplitResult.getBlockAfter();
            CaseValues oldKey = caseBlocks.get(caseBlock);
            if (oldKey == null) {
                caseBlocks.put(caseBlock, value);
            } else {
                // case with multiple values
                oldKey.merge(value);
            }
        }

        for (Map.Entry<BasicBlock, CaseValues> entry : caseBlocks.entrySet()) {
            BasicBlock caseBlock = entry.getKey();
            CaseValues value = entry.getValue();
            cfg.addEdge(switchBlock, caseBlock).setValue(value);
        }

        // necessary to tag the switch block after all case have been splitted
        switchBlock.setType(BasicBlockType.SWITCH);

        LOGGER.log(Level.FINER, "  Switch : switch={0}, cases={1}",
                new Object[]{switchBlock, caseBlocks});
    }
}