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

import fr.jamgotchian.abcd.core.util.Collections3;
import fr.jamgotchian.abcd.core.util.Sets;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class DominatorsFinder<N, E> extends ForwardDataFlowAnalysis<N, E, Set<N>>  {

    DominatorsFinder(DirectedGraph<N, E> graph, N entryNode) {
        super("Dominators", graph, entryNode);
    }

    @Override
    public Set<N> getInitValue(N node, boolean isEntryNode) {
        if (isEntryNode) {
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
    public Set<N> applyTranferFunction(N node, Set<N> inValue) {
        Set<N> outValue = new HashSet<>((inValue == null ? 0 : inValue.size()) + 1);
        outValue.add(node);
        if (inValue != null) {
            outValue.addAll(inValue);
        }
        return outValue;
    }

    @Override
    public boolean valuesEqual(Set<N> value1, Set<N> value2) {
        return Collections3.equals(value1, value2);
    }
}
