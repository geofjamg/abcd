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
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.Collection;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LoopRegion extends AbstractRegion {

    private final LoopType loopType;

    private final Region loopRegion;

    private final Edge backEdge;

    public LoopRegion(LoopType loopType, Edge backEdge, Region loopRegion) {
        if (loopType == null) {
            throw new ABCDException("loopType == null");
        }
        if (backEdge == null) {
            throw new ABCDException("backEdge == null");
        }
        if (loopRegion == null) {
            throw new ABCDException("loopRegion == null");
        }
        this.loopType = loopType;
        this.backEdge = backEdge;
        this.loopRegion = loopRegion;
    }

    public RegionType getType() {
        return RegionType.LOOP;
    }

    public LoopType getLoopType() {
        return loopType;
    }

    public Region getEntryRegion() {
        return loopRegion;
    }

    public Region getExitRegionIfUnique() {
        return loopRegion;
    }

    public Region getLoopRegion() {
        return loopRegion;
    }

    public Edge getBackEdge() {
        return backEdge;
    }

    public Collection<Region> getChildRegions() {
        return Sets.newHashSet(loopRegion);
    }

    public Collection<Edge> getChildEdges() {
        return Sets.newHashSet(backEdge);
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveIncomingEdges(graph, loopRegion, this);
        graph.removeEdge(backEdge);
        switch (loopType) {
            case WHILE:
                Region exitRegion = ((IfBreakRegion) loopRegion.getEntryRegion()).getBreakRegion();
                graph.addEdge(this, exitRegion, new EdgeImpl());
                break;

            case DO_WHILE:
                Regions.moveOutgoingEdges(graph, loopRegion, this);
                break;

            default:
                throw new AssertionError();
        }
        graph.removeVertex(loopRegion);
    }
}
