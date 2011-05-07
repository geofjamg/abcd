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

import com.google.common.collect.Sets;
import fr.jamgotchian.abcd.core.controlflow.BackwardDataFlowAnalysis;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.util.Collections3;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LiveVariablesAnalysis
    extends BackwardDataFlowAnalysis<BasicBlock, Edge, Set<Variable>> {

    public LiveVariablesAnalysis(ControlFlowGraph CFG) {
        super(CFG.getGraph(), CFG.getExitBlock());
    }

    private static Set<Variable> getDefs(BasicBlock block) {
        Set<Variable> defs = new HashSet<Variable>();
        for (TACInst inst : ((AnalysisData) block.getData()).getInstructions()) {
            if (inst.getDef() != null) {
                defs.add(inst.getDef());
            }
        }
        return defs;
    }

    private static Set<Variable> getUses(BasicBlock block) {
        Set<Variable> uses = new HashSet<Variable>();
        for (TACInst inst : ((AnalysisData) block.getData()).getInstructions()) {
            uses.addAll(inst.getUses());
        }
        return uses;
    }

    @Override
    protected Set<Variable> getInitValue(BasicBlock node, boolean isExitNode) {
        return new HashSet<Variable>();
    }

    @Override
    protected Set<Variable> combineValues(Set<Variable> value1, Set<Variable> value2) {
        return Sets.intersection(value1, value2);
    }

    @Override
    protected Set<Variable> applyTranferFunction(BasicBlock node, Set<Variable> outValue) {
        Set<Variable> inValue = new HashSet<Variable>(outValue);
        inValue.removeAll(getDefs(node));
        inValue.addAll(getUses(node));
        return inValue;
    }

    @Override
    protected boolean valuesEqual(Set<Variable> value1, Set<Variable> value2) {
        return Collections3.sameContent(value1, value2);
    }
}
