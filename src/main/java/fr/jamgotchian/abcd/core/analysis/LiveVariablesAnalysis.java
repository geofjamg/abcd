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
import fr.jamgotchian.abcd.core.tac.model.LocalVariable;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.util.Collections3;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LiveVariablesAnalysis extends BackwardDataFlowAnalysis<BasicBlock, Edge, Set<LocalVariable>> {

    private static final Logger logger = Logger.getLogger(LiveVariablesAnalysis.class.getName());

    public LiveVariablesAnalysis(ControlFlowGraph CFG) {
        super(CFG.getGraph(), CFG.getExitBlock());
    }

    private static Set<LocalVariable> getDefs(BasicBlock block) {
        Set<LocalVariable> defs = new HashSet<LocalVariable>();
        for (TACInst inst : ((AnalysisData) block.getData()).getInstructions()) {
            if (inst.getDef() != null) {
                defs.add(inst.getDef());
            }
        }
        return defs;
    }

    private static Set<LocalVariable> getUses(BasicBlock block) {
        Set<LocalVariable> uses = new HashSet<LocalVariable>();
        for (TACInst inst : ((AnalysisData) block.getData()).getInstructions()) {
            uses.addAll(inst.getUses());
        }
        return uses;
    }

    @Override
    public Map<BasicBlock, Set<LocalVariable>> analyse() {
        logger.log(Level.FINER, "Live variables analysis :");
        Map<BasicBlock, Set<LocalVariable>> liveVariables = super.analyse();
        for (Map.Entry<BasicBlock, Set<LocalVariable>> entry : liveVariables.entrySet()) {
            logger.log(Level.FINER, "  {0} : {1}",
                    new Object[] {entry.getKey(), entry.getValue()});
        }
        return liveVariables;
    }

    @Override
    protected Set<LocalVariable> getInitValue(BasicBlock node, boolean isExitNode) {
        return new HashSet<LocalVariable>();
    }

    @Override
    protected Set<LocalVariable> combineValues(Set<LocalVariable> value1,
                                               Set<LocalVariable> value2) {
        return Sets.intersection(value1, value2);
    }

    @Override
    protected Set<LocalVariable> applyTranferFunction(BasicBlock node,
                                                      Set<LocalVariable> outValue) {
        Set<LocalVariable> inValue = new HashSet<LocalVariable>(outValue);
        inValue.removeAll(getDefs(node));
        inValue.addAll(getUses(node));
        return inValue;
    }

    @Override
    protected boolean valuesEqual(Set<LocalVariable> value1,
                                  Set<LocalVariable> value2) {
        return Collections3.sameContent(value1, value2);
    }
}
