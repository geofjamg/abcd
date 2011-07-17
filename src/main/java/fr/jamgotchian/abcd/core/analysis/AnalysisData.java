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

package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.controlflow.BasicBlockData;
import fr.jamgotchian.abcd.core.tac.model.DefInst;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.TACInstSeq;
import fr.jamgotchian.abcd.core.tac.util.VariableStack;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class AnalysisData implements BasicBlockData {

    private final TACInstSeq instructions;

    private VariableStack inputStack;

    private VariableStack outputStack;

    public AnalysisData() {
        instructions = new TACInstSeq();
    }

    public TACInstSeq getInstructions() {
        return instructions;
    }

    public int getInstructionCount() {
        int count = 0;
        for (TACInst inst : instructions) {
            if (inst instanceof DefInst
                    && !((DefInst) inst).getResult().isTemporary()) {
                count++;
            }
        }
        return count;
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
}
