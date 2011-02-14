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

package fr.jamgotchian.abcd.core.controlflow;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Iterative data flow analysis framework
 * 
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class DataFlowAnalysis<V> {

    protected abstract BasicBlock getEntryBlock();

    protected abstract Collection<BasicBlock> getPredecessorsOf(BasicBlock block);

    protected abstract V getInitValue(BasicBlock block, boolean isStartBlock);

    protected abstract V combineValues(V value1, V value2);

    protected abstract V applyTranferFunction(BasicBlock block, V inValue);

    protected abstract boolean valuesEqual(V value1, V value2);

    private final ControlFlowGraph graph;

    public DataFlowAnalysis(ControlFlowGraph graph) {
        this.graph = graph;
    }

    public ControlFlowGraph getGraph() {
        return graph;
    }

    public Map<BasicBlock, V> analyse() {
        Map<BasicBlock, V> values = new HashMap<BasicBlock, V>();

        for (BasicBlock block : graph.getBasicBlocks()) {
           values.put(block, getInitValue(block, block.equals(getEntryBlock())));
        }

        boolean change = true;
        while (change) {
            change = false;
            for (BasicBlock b : graph.getBasicBlocks()) {
                if (!b.equals(getEntryBlock())) {
                    V outBefore = values.get(b);
                    V in = null;
                    for (BasicBlock p : getPredecessorsOf(b)) {
                        if (in == null) {
                            in = values.get(p);
                        } else {
                            in = combineValues(in, values.get(p));
                        }
                    }
                    V out = applyTranferFunction(b, in);
                    if (!valuesEqual(outBefore, out)) {
                        values.put(b, out);
                        change = true;
                    }
                }
            }
        }
        return values;
    }
}
