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

package fr.jamgotchian.abcd.graph;

import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class ForwardDataFlowAnalysis<N, E, V> {

    private static final Logger LOGGER = LoggerFactory.getLogger(ForwardDataFlowAnalysis.class);

    private final String name;

    private final DirectedGraph<N, E> graph;

    private final N entryNode;

    public ForwardDataFlowAnalysis(String name, DirectedGraph<N, E> graph, N entryNode) {
        this.name = name;
        this.graph = graph;
        this.entryNode = entryNode;
    }

    public DirectedGraph<N, E> getGraph() {
        return graph;
    }

    public Map<N, V> analyse() {
        LOGGER.trace("Begin forward dataflow analysis {}", name);

        Map<N, V> values = new HashMap<>();

        for (N node : graph.getVertices()) {
            V initValue = getInitValue(node, node.equals(entryNode));
            values.put(node, initValue);
            LOGGER.trace("out[{}]={}", node, initValue);
        }

        boolean change = true;
        while (change) {
            change = false;
            for (N n : graph.getVertices()) {
                if (!n.equals(entryNode)) {
                    V outBefore = values.get(n);
                    V in = null;
                    for (N p : graph.getPredecessorsOf(n)) {
                        if (in == null) {
                            in = values.get(p);
                        } else {
                            in = combineValues(in, values.get(p));
                        }
                    }
                    V out = applyTranferFunction(n, in);
                    if (!valuesEqual(outBefore, out)) {
                        values.put(n, out);
                        change = true;
                        LOGGER.trace("out[{}]={}", new Object[] {n, out});
                    }
                }
            }
        }

        LOGGER.trace("End forward dataflow analysis {}", name);

        return values;
    }

    protected abstract V getInitValue(N node, boolean isEntryNode);

    protected abstract V combineValues(V value1, V value2);

    protected abstract V applyTranferFunction(N node, V inValue);

    protected abstract boolean valuesEqual(V value1, V value2);
}