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

package fr.jamgotchian.abcd.core.graph;

import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class BackwardDataFlowAnalysis<N, E, V> {

    private static final Logger LOGGER = LoggerFactory.getLogger(BackwardDataFlowAnalysis.class);

    private final String name;

    private final DirectedGraph<N, E> graph;

    private final N exitNode;

    public BackwardDataFlowAnalysis(String name, DirectedGraph<N, E> graph, N exitNode) {
        this.name = name;
        this.graph = graph;
        this.exitNode = exitNode;
    }

    public DirectedGraph<N, E> getGraph() {
        return graph;
    }

    public Map<N, V> analyse() {
        LOGGER.trace("Begin backward dataflow analysis {}", name);

        Map<N, V> values = new HashMap<N, V>();

        for (N node : graph.getVertices()) {
            V initValue = getInitValue(node, node.equals(exitNode));
            values.put(node, initValue);
            LOGGER.trace("in[{}]={}", node, initValue);
        }

        boolean change = true;
        while (change) {
            change = false;
            for (N n : graph.getVertices()) {
                if (!n.equals(exitNode)) {
                    V inBefore = values.get(n);
                    V out = null;
                    for (N s : graph.getSuccessorsOf(n)) {
                        if (out == null) {
                            out = values.get(s);
                        } else {
                            out = combineValues(out, values.get(s));
                        }
                    }
                    V in = applyTranferFunction(n, out);
                    if (!valuesEqual(inBefore, in)) {
                        values.put(n, in);
                        change = true;
                        LOGGER.trace("in[{}]={}", n, in);
                    }
                }
            }
        }

        LOGGER.trace("End backward dataflow analysis {}", name);

        return values;
    }

    protected abstract V getInitValue(N node, boolean isExitNode);

    protected abstract V combineValues(V value1, V value2);

    protected abstract V applyTranferFunction(N node, V outValue);

    protected abstract boolean valuesEqual(V value1, V value2);
}
