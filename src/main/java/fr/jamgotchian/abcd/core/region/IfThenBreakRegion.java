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
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfThenBreakRegion extends AbstractRegion {

    private final Region ifRegion;

    private final Region breakTargetRegion;

    private final Edge elseEdge;

    private final Edge thenBreakEdge;

    private Region afterThenRegion;

    private Edge afterThenEdge;

    private Region beforeThenRegion;

    private Edge beforeThenEdge;

    private final boolean invertCond;

    static IfThenBreakRegion newInstance(Region ifRegion, Region breakTargetRegion,
                                         Edge elseEdge, Edge thenBreakEdge,
                                         boolean invertCond) {
        return new IfThenBreakRegion(ifRegion, breakTargetRegion, elseEdge, thenBreakEdge,
                                     null, null, null, null,
                                     invertCond);
    }

    static IfThenBreakRegion newInstance2(Region ifRegion, Region breakTargetRegion,
                                          Edge elseEdge, Edge thenBreakEdge,
                                          Region afterThenRegion, Edge afterThenEdge,
                                          boolean invertCond) {
        return new IfThenBreakRegion(ifRegion, breakTargetRegion, elseEdge, thenBreakEdge,
                                     null, null, afterThenRegion, afterThenEdge,
                                     invertCond);
    }

    static IfThenBreakRegion newInstance3(Region ifRegion, Region breakTargetRegion,
                                          Edge elseEdge, Edge thenBreakEdge,
                                          Region beforeThenRegion, Edge beforeThenEdge,
                                          Region afterThenRegion, Edge afterThenEdge,
                                          boolean invertCond) {
        return new IfThenBreakRegion(ifRegion, breakTargetRegion, elseEdge, thenBreakEdge,
                                     beforeThenRegion, beforeThenEdge, afterThenRegion, afterThenEdge,
                                     invertCond);
    }

    private IfThenBreakRegion(Region ifRegion, Region breakTargetRegion, Edge elseEdge, Edge thenBreakEdge,
                      Region beforeThenRegion, Edge beforeThenEdge, Region afterThenRegion, Edge afterThenEdge,
                      boolean invertCond) {
        this.ifRegion = ifRegion;
        this.breakTargetRegion = breakTargetRegion;
        this.elseEdge = elseEdge;
        this.thenBreakEdge = thenBreakEdge;
        this.beforeThenRegion = beforeThenRegion;
        this.beforeThenEdge = beforeThenEdge;
        this.afterThenRegion = afterThenRegion;
        this.afterThenEdge = afterThenEdge;
        this.invertCond = invertCond;
    }

    public Region getIfRegion() {
        return ifRegion;
    }

    public Region getBreakTargetRegion() {
        return breakTargetRegion;
    }

    public Edge getThenBreakEdge() {
        return thenBreakEdge;
    }

    public Edge getElseEdge() {
        return elseEdge;
    }

    public Region getAfterThenRegion() {
        return afterThenRegion;
    }

    public void setAfterThenRegion(Region afterThenRegion) {
        this.afterThenRegion = afterThenRegion;
    }

    public Edge getAfterThenEdge() {
        return afterThenEdge;
    }

    public void setAfterThenEdge(Edge afterThenEdge) {
        this.afterThenEdge = afterThenEdge;
    }

    public Region getBeforeThenRegion() {
        return beforeThenRegion;
    }

    public Edge getBeforeThenEdge() {
        return beforeThenEdge;
    }

    public void setBeforeThenRegion(Region beforeThenRegion) {
        this.beforeThenRegion = beforeThenRegion;
    }

    public void setBeforeThenEdge(Edge beforeThenEdge) {
        this.beforeThenEdge = beforeThenEdge;
    }

    public boolean isInvertCond() {
        return invertCond;
    }

    public RegionType getType() {
        return RegionType.IF_THEN_BREAK;
    }

    public Region getEntryRegion() {
        return ifRegion;
    }

    public Region getExitRegion() {
        return ifRegion;
    }

    public Collection<Region> getChildRegions() {
        Set<Region> regions = new HashSet<Region>(1);
        regions.add(ifRegion);
        if (beforeThenRegion != null) {
            regions.add(beforeThenRegion);
        }
        if (afterThenRegion != null) {
            regions.add(afterThenRegion);
        }
        return regions;
    }

    public Collection<Edge> getChildEdges() {
        Set<Edge> edges = new HashSet<Edge>(1);
        edges.add(thenBreakEdge);
        if (beforeThenEdge != null) {
            edges.add(beforeThenEdge);
        }
        if (afterThenEdge != null) {
            edges.add(afterThenEdge);
        }
        return edges;
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        // change break target region to unassigned
        if (breakTargetRegion != null) {
            breakTargetRegion.setBreakTargetStatus(BreakTargetStatus.UNASSIGNED);
        }
        graph.addVertex(this);
        Region elseRegion = graph.getEdgeTarget(elseEdge);
        Regions.moveHandlers(graph, ifRegion, this);
        graph.removeEdge(elseEdge);
        graph.removeEdge(thenBreakEdge);
        if (beforeThenEdge != null) {
            graph.removeEdge(beforeThenEdge);
        }
        if (afterThenEdge != null) {
            graph.removeEdge(afterThenEdge);
        }
        Regions.moveIncomingEdges(graph, ifRegion, this);
        graph.removeVertex(ifRegion);
        if (beforeThenRegion != null) {
            graph.removeVertex(beforeThenRegion);
        }
        if (afterThenRegion != null) {
            graph.removeVertex(afterThenRegion);
        }
        graph.addEdge(this, elseRegion.equals(ifRegion) ? this : elseRegion, elseEdge);
    }
}
