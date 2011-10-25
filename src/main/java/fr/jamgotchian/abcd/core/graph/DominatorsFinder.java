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
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class DominatorsFinder<N, E> extends ForwardDataFlowAnalysis<N, E, Set<N>>  {

    DominatorsFinder(DirectedGraph<N, E> graph, N entryNode) {
        super(graph, entryNode);
    }

    @Override
    public Set<N> getInitValue(N node, boolean isEntryNode) {
        if (isEntryNode) {
            return com.google.common.collect.Sets.newHashSet(node);
        } else {
            return new HashSet<N>(getGraph().getVertices());
        }
    }

    @Override
    public Set<N> combineValues(Set<N> value1, Set<N> value2) {
        return Sets.intersection(value1 == null ? Collections.<N>emptySet() : value1,
                                 value2 == null ? Collections.<N>emptySet() : value2);
    }

    @Override
    public Set<N> applyTranferFunction(N node, Set<N> inValue) {
        Set<N> outValue = new HashSet<N>();
        outValue.add(node);
        if (inValue != null) {
            outValue.addAll(inValue);
        }
        return outValue;
    }

    @Override
    public boolean valuesEqual(Set<N> value1, Set<N> value2) {
        return Collections3.sameContent(value1, value2);
    }
}
