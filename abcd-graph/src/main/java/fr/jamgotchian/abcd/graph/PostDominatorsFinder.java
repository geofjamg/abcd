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

import com.google.common.collect.Sets;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class PostDominatorsFinder<N ,E> extends BackwardDataFlowAnalysis<N, E, Set<N>>  {

    PostDominatorsFinder(DirectedGraph<N, E> graph, N exitNode) {
        super("Post-dominators", graph, exitNode);
    }

    @Override
    public Set<N> getInitValue(N node, boolean isExitNode) {
        if (isExitNode) {
            Set<N> initValue = new HashSet<>(1);
            initValue.add(node);
            return initValue;
        } else {
            return new HashSet<>(getGraph().getVertices());
        }
    }

    @Override
    public Set<N> combineValues(Set<N> value1, Set<N> value2) {
        return Sets.intersection(value1, value2);
    }

    @Override
    public Set<N> applyTranferFunction(N node, Set<N> outValue) {
        Set<N> inValue = new HashSet<>((outValue == null ? 0 : outValue.size()) + 1);
        inValue.add(node);
        if (outValue != null) {
            inValue.addAll(outValue);
        }
        return inValue;
    }

    @Override
    public boolean valuesEqual(Set<N> value1, Set<N> value2) {
        return Objects.equals(value1, value2);
    }
}
