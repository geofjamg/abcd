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
import java.util.Arrays;
import java.util.Collection;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfThenElseRegion extends AbstractRegion {

    private final Edge beforeThenEdge;

    private final Edge afterThenEdge;

    private final Edge beforeElseEdge;

    private final Edge afterElseEdge;

    private final Region ifRegion;

    private final Region thenRegion;

    private final Region elseRegion;

    IfThenElseRegion(Edge beforeThenEdge, Edge afterThenEdge,
                     Edge beforeElseEdge, Edge afterElseEdge,
                     Region ifRegion, Region thenRegion, Region elseRegion) {
        if (beforeThenEdge == null) {
            throw new IllegalArgumentException("beforeThenEdge == null");
        }
        if (afterThenEdge == null) {
            throw new IllegalArgumentException("afterThenEdge == null");
        }
        if (beforeElseEdge == null) {
            throw new IllegalArgumentException("beforeElseEdge == null");
        }
        if (afterElseEdge == null) {
            throw new IllegalArgumentException("afterElseEdge == null");
        }
        if (ifRegion == null) {
            throw new IllegalArgumentException("ifRegion == null");
        }
        if (thenRegion == null) {
            throw new IllegalArgumentException("thenRegion == null");
        }
        if (elseRegion == null) {
            throw new IllegalArgumentException("elseRegion == null");
        }
        this.beforeThenEdge = beforeThenEdge;
        this.afterThenEdge = afterThenEdge;
        this.beforeElseEdge = beforeElseEdge;
        this.afterElseEdge = afterElseEdge;
        this.ifRegion = ifRegion;
        this.thenRegion = thenRegion;
        this.elseRegion = elseRegion;
        ifRegion.setParent(this);
        thenRegion.setParent(this);
        elseRegion.setParent(this);
    }

    public RegionType getType() {
        return RegionType.IF_THEN_ELSE;
    }

    public Region getEntryRegion() {
         return ifRegion;
    }

    public Region getExitRegion() {
        return null;
    }

    public Region getIfRegion() {
        return ifRegion;
    }

    public Region getThenRegion() {
        return thenRegion;
    }

    public Region getElseRegion() {
        return elseRegion;
    }

    public Edge getAfterElseEdge() {
        return afterElseEdge;
    }

    public Edge getAfterThenEdge() {
        return afterThenEdge;
    }

    public Edge getBeforeElseEdge() {
        return beforeElseEdge;
    }

    public Edge getBeforeThenEdge() {
        return beforeThenEdge;
    }

    public Collection<Region> getChildRegions() {
        return Arrays.asList(ifRegion, thenRegion, elseRegion);
    }

    public Collection<Edge> getChildEdges() {
        return Sets.newHashSet(beforeThenEdge, afterThenEdge, beforeElseEdge,
                               afterElseEdge);
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveHandlers(graph, ifRegion, this);
        Regions.moveIncomingEdges(graph, ifRegion, this);
        graph.addEdge(this, graph.getEdgeTarget(afterThenEdge), new EdgeImpl());
        graph.removeEdge(beforeThenEdge);
        graph.removeEdge(beforeElseEdge);
        graph.removeEdge(afterThenEdge);
        graph.removeEdge(afterElseEdge);
        graph.removeVertex(ifRegion);
        graph.removeVertex(thenRegion);
        graph.removeVertex(elseRegion);
    }
}
