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
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DoWhileLoopRegion extends AbstractRegion {

    private final Region loopRegion;

    private final Edge backEdge;

    private final Edge exitEdge;

    public DoWhileLoopRegion(Edge backEdge, Region loopRegion, Edge exitEdge) {
        if (backEdge == null) {
            throw new IllegalArgumentException("backEdge == null");
        }
        if (loopRegion == null) {
            throw new IllegalArgumentException("loopRegion == null");
        }
        if (exitEdge == null) {
            throw new IllegalArgumentException("exitEdge == null");
        }
        this.backEdge = backEdge;
        this.loopRegion = loopRegion;
        this.exitEdge = exitEdge;
        loopRegion.setParent(this);
    }

    public RegionType getType() {
        return RegionType.DO_WHILE_LOOP;
    }

    public Region getEntryRegion() {
        return loopRegion;
    }

    public Region getExitRegion() {
        return loopRegion;
    }

    public Region getLoopRegion() {
        return loopRegion;
    }

    public List<Region> getChildRegions() {
        return Collections.singletonList(loopRegion);
    }

    public Collection<Edge> getChildEdges() {
        return Collections.singleton(backEdge);
    }

    public void reduce(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveHandlers(graph, loopRegion, this);
        graph.removeEdge(backEdge);
        Regions.moveIncomingEdges(graph, loopRegion, this);
        exitEdge.setValue(null);
        Region exitRegion = graph.getEdgeTarget(exitEdge);
        graph.removeVertex(loopRegion);
        graph.addEdge(this, exitRegion, exitEdge);
    }
}
