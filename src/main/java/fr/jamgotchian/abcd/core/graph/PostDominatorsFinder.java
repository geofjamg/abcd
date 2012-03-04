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

import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class PostDominatorsFinder<N ,E> extends BackwardDataFlowAnalysis<N, E, Set<N>>  {

    private final DominatorsFinder<N, E> dominatorsFinder;

    PostDominatorsFinder(DirectedGraph<N, E> graph, N exitNode) {
        super("Post-dominators", graph, exitNode);
        dominatorsFinder = new DominatorsFinder<N, E>(graph, exitNode);
    }

    @Override
    public Set<N> getInitValue(N node, boolean isExitNode) {
        return dominatorsFinder.getInitValue(node, isExitNode);
    }

    @Override
    public Set<N> combineValues(Set<N> value1, Set<N> value2) {
        return dominatorsFinder.combineValues(value1, value2);
    }

    @Override
    public Set<N> applyTranferFunction(N node, Set<N> outValue) {
        return dominatorsFinder.applyTranferFunction(node, outValue);
    }

    @Override
    public boolean valuesEqual(Set<N> value1, Set<N> value2) {
        return dominatorsFinder.valuesEqual(value1, value2);
    }
}
