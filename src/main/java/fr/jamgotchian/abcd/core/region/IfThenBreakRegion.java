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
import java.util.Collections;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfThenBreakRegion extends AbstractRegion {

    private final Region ifRegion;

    private final Region breakRegion;

    private final Edge elseEdge;

    private final Edge thenEdge;

    private final Region thenRegion;

    private final Edge thenEdge2;

    private final boolean invertCond;

    public IfThenBreakRegion(Region ifRegion, Region breakRegion, Edge elseEdge, Edge thenEdge,
                             Region thenRegion, Edge thenEdge2, boolean invertCond) {
        this.ifRegion = ifRegion;
        this.breakRegion = breakRegion;
        this.elseEdge = elseEdge;
        this.thenEdge = thenEdge;
        this.thenRegion = thenRegion;
        this.thenEdge2 = thenEdge2;
        this.invertCond = invertCond;
    }

    public Region getIfRegion() {
        return ifRegion;
    }

    public Region getBreakRegion() {
        return breakRegion;
    }

    public Edge getElseEdge() {
        return elseEdge;
    }

    public Edge getThenEdge() {
        return thenEdge;
    }

    public Region getThenRegion() {
        return thenRegion;
    }

    public Edge getThenEdge2() {
        return thenEdge2;
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

    public Region getExitRegionIfUnique() {
        return null;
    }

    public Collection<Region> getChildRegions() {
        if (thenRegion == null) {
            return Collections.singleton(ifRegion);
        } else {
            return Arrays.asList(ifRegion, thenRegion);
        }
    }

    public Collection<Edge> getChildEdges() {
        if (thenEdge2 == null) {
            return Sets.newHashSet(thenEdge, elseEdge);
        } else {
            return Sets.newHashSet(thenEdge, elseEdge, thenEdge2);
        }
    }

    @Override
    public void addBreakRegion(Collection<Region> regions) {
        super.addBreakRegion(regions);
        regions.add(breakRegion);
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveHandlers(graph, ifRegion, this);
        Region joinRegion = graph.getEdgeTarget(elseEdge);
        graph.removeEdge(elseEdge);
        graph.removeEdge(thenEdge);
        if (thenEdge2 != null) {
            graph.removeEdge(thenEdge2);
        }
        Regions.moveIncomingEdges(graph, ifRegion, this);
        graph.removeVertex(ifRegion);
        if (thenRegion != null) {
            graph.removeVertex(thenRegion);
        }
        graph.addEdge(this, joinRegion, new EdgeImpl());
    }
}
