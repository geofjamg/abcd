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
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.util.VariableStack;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class AnalysisData implements BasicBlockData {

    private VariableStack inputStack2;

    private VariableStack outputStack2;

    private final List<TACInst> instructions;

    public AnalysisData() {
        instructions = new ArrayList<TACInst>();
    }

    public List<TACInst> getInstructions() {
        return instructions;
    }

    public int getInstructionCount() {
        return instructions.size();
    }

    public TACInst getLastInst() {
        return instructions.isEmpty() ? null : instructions.get(instructions.size()-1);
    }

    public VariableStack getInputStack2() {
        return inputStack2;
    }

    public void setInputStack2(VariableStack inputStack2) {
        this.inputStack2 = inputStack2;
    }

    public VariableStack getOutputStack2() {
        return outputStack2;
    }

    public void setOutputStack2(VariableStack outputStack2) {
        this.outputStack2 = outputStack2;
    }
}
