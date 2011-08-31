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

package fr.jamgotchian.abcd.core.region;

import com.google.common.collect.Sets;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.Collection;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfThenJoinRegion extends IfThenRegion {

    private final Edge joinThenEdge;

    IfThenJoinRegion(Edge thenEdge, Edge joinThenEdge, Edge elseEdge,
                     Region ifRegion, Region thenRegion, boolean mustInvertCondition) {
        super(thenEdge, elseEdge, ifRegion, thenRegion, mustInvertCondition);
        if (joinThenEdge == null) {
            throw new IllegalArgumentException("joinThenEdge == null");
        }
        this.joinThenEdge = joinThenEdge;
    }

    public RegionType getType() {
        return RegionType.IF_THEN_JOIN;
    }

    public Collection<Edge> getChildEdges() {
        return Sets.newHashSet(thenEdge, joinThenEdge, elseEdge);
    }

    public void reduce(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveHandlers(graph, ifRegion, this);
        Regions.moveIncomingEdges(graph, ifRegion, this);
        graph.addEdge(this, graph.getEdgeTarget(joinThenEdge), new EdgeImpl());
        graph.removeEdge(thenEdge);
        graph.removeEdge(joinThenEdge);
        graph.removeEdge(elseEdge);
        graph.removeVertex(ifRegion);
        graph.removeVertex(thenRegion);
    }
}