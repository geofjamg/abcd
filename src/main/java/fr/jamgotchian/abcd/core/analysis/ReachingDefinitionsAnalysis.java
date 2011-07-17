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

import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.ForwardDataFlowAnalysis;
import fr.jamgotchian.abcd.core.controlflow.DefInst;
import fr.jamgotchian.abcd.core.controlflow.TACInst;
import fr.jamgotchian.abcd.core.controlflow.Variable;
import fr.jamgotchian.abcd.core.controlflow.VariableID;
import fr.jamgotchian.abcd.core.util.Collections3;
import fr.jamgotchian.abcd.core.util.Sets;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ReachingDefinitionsAnalysis extends ForwardDataFlowAnalysis<BasicBlock, Edge, Set<Integer>> {

    private static final Logger logger
            = Logger.getLogger(ReachingDefinitionsAnalysis.class.getName());

    private final Map<Integer, VariableID> defs = new HashMap<Integer, VariableID>();

    public ReachingDefinitionsAnalysis(ControlFlowGraph CFG) {
        super(CFG.getGraph(), CFG.getEntryBlock());

        for (BasicBlock block : CFG.getBasicBlocks()) {
            for (TACInst inst : block.getInstructions()) {
                if (inst instanceof DefInst) {
                    DefInst def = (DefInst) inst;
                    defs.put(def.getDefID(), def.getResult().getID());
                }
            }
        }
    }

    @Override
    public Map<BasicBlock, Set<Integer>> analyse() {
        Map<BasicBlock, Set<Integer>> reachingDefs  = super.analyse();
        logger.log(Level.FINER, "Reaching defintions analysis :");
        for (Map.Entry<BasicBlock, Set<Integer>> entry : reachingDefs.entrySet()) {
            logger.log(Level.FINER, "  {0} : {1}",
                    new Object[] {entry.getKey(), entry.getValue()});
        }
        return reachingDefs;
    }

    private void getGensAndKills(BasicBlock block, Set<Integer> inValue,
                                 Set<Integer> gens, Set<Integer> kills) {
        Map<VariableID, Integer> reachingDef = new HashMap<VariableID, Integer>();
        for (Integer defID : inValue) {
            reachingDef.put(defs.get(defID), defID);
        }

        for (TACInst inst : block.getInstructions()) {
            if (inst instanceof DefInst) {
                DefInst def = (DefInst) inst;
                Variable result = def.getResult();
                if (reachingDef.containsKey(result.getID())) {
                    kills.add(reachingDef.get(result.getID()));
                } else {
                    gens.add(def.getDefID());
                }
            }
        }
    }

    @Override
    protected Set<Integer> getInitValue(BasicBlock node, boolean isEntryNode) {
        return new HashSet<Integer>();
    }

    @Override
    protected Set<Integer> combineValues(Set<Integer> value1, Set<Integer> value2) {
        return Sets.intersection(value1, value2);
    }

    @Override
    protected Set<Integer> applyTranferFunction(BasicBlock node, Set<Integer> inValue) {
        Set<Integer> outValue = new HashSet<Integer>(inValue);
        Set<Integer> gens = new HashSet<Integer>();
        Set<Integer> kills = new HashSet<Integer>();
        getGensAndKills(node, inValue, gens, kills);
        outValue.removeAll(kills);
        outValue.addAll(gens);
        return outValue;
    }

    @Override
    protected boolean valuesEqual(Set<Integer> value1, Set<Integer> value2) {
        return Collections3.sameContent(value1, value2);
    }
}
