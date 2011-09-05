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

import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfThenElseJoinRegion extends IfThenElseRegion {

    private final Edge joinThenEdge;

    private final Edge joinElseEdge;

    IfThenElseJoinRegion(Edge thenEdge, Edge joinThenEdge, Edge elseEdge, Edge joinElseEdge,
                         Region ifRegion, Region thenRegion, Region elseRegion) {
        super(thenEdge, elseEdge, ifRegion, thenRegion, elseRegion);
        if (joinThenEdge == null) {
            throw new IllegalArgumentException("joinThenEdge == null");
        }
        if (joinElseEdge == null) {
            throw new IllegalArgumentException("joinElseEdge == null");
        }
        this.joinThenEdge = joinThenEdge;
        this.joinElseEdge = joinElseEdge;
    }

    public RegionType getType() {
        return RegionType.IF_THEN_ELSE_JOIN;
    }

    public void reduce(RegionGraph graph) {
        graph.addRegion(this);
        Regions.moveHandlers(graph, ifRegion, this);
        Regions.moveIncomingEdges(graph, ifRegion, this);
        graph.addEdge(this, graph.getEdgeTarget(joinThenEdge), new EdgeImpl());
        graph.removeEdge(thenEdge);
        graph.removeEdge(elseEdge);
        graph.removeEdge(joinThenEdge);
        graph.removeEdge(joinElseEdge);
        graph.removeRegion(ifRegion);
        graph.removeRegion(thenRegion);
        graph.removeRegion(elseRegion);
    }
}
