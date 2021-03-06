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
package fr.jamgotchian.abcd.core.ir;

import fr.jamgotchian.abcd.graph.BackwardDataFlowAnalysis;
import com.google.common.collect.Sets;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LiveVariablesAnalysis extends BackwardDataFlowAnalysis<BasicBlock, Edge, Set<Variable>> {

    private static final Logger LOGGER
            = LoggerFactory.getLogger(LiveVariablesAnalysis.class);

    public LiveVariablesAnalysis(ControlFlowGraph cfg) {
        super("Live variables", cfg.getGraph(), cfg.getExitBlock());
    }

    private static Set<Variable> getDefs(BasicBlock block) {
        Set<Variable> defs = new HashSet<>();
        for (IRInst inst : block.getInstructions()) {
            if (inst instanceof DefInst) {
                defs.add(((DefInst) inst).getResult());
            }
        }
        return defs;
    }

    private static Set<Variable> getUses(BasicBlock block) {
        Set<Variable> uses = new HashSet<>();
        for (IRInst inst : block.getInstructions()) {
            uses.addAll(inst.getUses());
        }
        return uses;
    }

    @Override
    public Map<BasicBlock, Set<Variable>> analyse() {
        LOGGER.trace("Live variables analysis :");
        Map<BasicBlock, Set<Variable>> liveVariables = super.analyse();
        for (Map.Entry<BasicBlock, Set<Variable>> entry : liveVariables.entrySet()) {
            LOGGER.trace("  {} : {}", entry.getKey(), entry.getValue());
        }
        return liveVariables;
    }

    @Override
    protected Set<Variable> getInitValue(BasicBlock node, boolean isExitNode) {
        return new HashSet<>();
    }

    @Override
    protected Set<Variable> combineValues(Set<Variable> value1, Set<Variable> value2) {
        return Sets.union(value1, value2);
    }

    @Override
    protected Set<Variable> applyTranferFunction(BasicBlock node,
                                                 Set<Variable> outValue) {
        Set<Variable> inValue = new HashSet<>(outValue);
        inValue.removeAll(getDefs(node));
        inValue.addAll(getUses(node));
        return inValue;
    }

    @Override
    protected boolean valuesEqual(Set<Variable> value1, Set<Variable> value2) {
        return Objects.equals(value1, value2);
    }
}
