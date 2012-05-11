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
package fr.jamgotchian.abcd.core.bytecode.dalvik;

import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraphBuilder;
import fr.jamgotchian.abcd.core.ir.ExceptionTable;
import fr.jamgotchian.abcd.core.ir.LocalVariableTable;
import org.jf.dexlib.Code.Instruction;
import org.jf.dexlib.Code.OffsetInstruction;
import org.jf.dexlib.CodeItem;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DalvikBytecodeControlFlowGraphBuilder extends ControlFlowGraphBuilder {

    private final CodeItem codeItem;

    private final CodeAddressManager addressManager;

    public DalvikBytecodeControlFlowGraphBuilder(String methodName,
                                                 CodeItem codeItem,
                                                 CodeAddressManager addressManager) {
        super(methodName);
        this.codeItem = codeItem;
        this.addressManager = addressManager;
    }

    @Override
    protected void analyseInstructions() {
        Instruction[] instructions = codeItem.getInstructions();
        for (int position = 0; position < instructions.length; position++) {
            Instruction instruction = instructions[position];
            switch (instruction.getFormat()) {
                case Format21t:
                case Format22t: {
                    OffsetInstruction jumpInst = (OffsetInstruction) instruction;
                    int targetPosition
                            = addressManager.getTargetPosition(position,
                                                               jumpInst.getTargetAddressOffset());
                    analyseJumpInst(position, targetPosition);
                    break;
                }

                case Format10t:
                case Format20t:
                case Format30t: {
                    OffsetInstruction gotoInst = (OffsetInstruction) instruction;
                    int targetPosition
                            = addressManager.getTargetPosition(position,
                                                               gotoInst.getTargetAddressOffset());
                    analyseGotoInst(position, targetPosition);
                    break;
                }

                case Format10x:
                case Format11x: {
                    analyseReturnInst(position);
                }
            }
        }
    }

    @Override
    protected int getInstructionCount() {
        return codeItem.getInstructions().length;
    }

    @Override
    protected GraphvizRenderer<BasicBlock> getGraphizRenderer() {
        return new DalvikBytecodeGraphvizRenderer(codeItem.getInstructions(),
                                                  addressManager);
    }

    @Override
    protected ExceptionTable getExceptionTable() {
        // TODO
        return new ExceptionTable();
    }

    @Override
    protected LocalVariableTable getLocalVariableTable() {
        // TODO
        return new LocalVariableTable();
    }

}
