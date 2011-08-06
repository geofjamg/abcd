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
public class LoopRegion extends AbstractRegion {

    private final LoopType loopType;

    private final Region loopRegion;

    private final Edge backEdge;

    private final int loopID;

    public LoopRegion(LoopType loopType, Edge backEdge, Region loopRegion, int loopID) {
        if (loopType == null) {
            throw new IllegalArgumentException("loopType == null");
        }
        if (backEdge == null) {
            throw new IllegalArgumentException("backEdge == null");
        }
        if (loopRegion == null) {
            throw new IllegalArgumentException("loopRegion == null");
        }
        this.loopType = loopType;
        this.backEdge = backEdge;
        this.loopRegion = loopRegion;
        this.loopID = loopID;
        loopRegion.setParent(this);
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

    public Region getExitRegion() {
        return loopRegion;
    }

    public Region getLoopRegion() {
        return loopRegion;
    }

    public Edge getBackEdge() {
        return backEdge;
    }

    public int getLoopID() {
        return loopID;
    }

    public List<Region> getChildRegions() {
        return Collections.singletonList(loopRegion);
    }

    public Collection<Edge> getChildEdges() {
        return Collections.singleton(backEdge);
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveHandlers(graph, loopRegion, this);
        Regions.moveIncomingEdges(graph, loopRegion, this);
        graph.removeEdge(backEdge);
        graph.removeVertex(loopRegion);
    }
}
