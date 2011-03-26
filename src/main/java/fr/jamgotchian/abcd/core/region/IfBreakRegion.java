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
import java.util.Collections;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfBreakRegion extends AbstractRegion {

    private final Region ifRegion;
    
    private final Region breakRegion;

    private final Edge breakEdge;
    
    private final Edge elseEdge;

    private final boolean invertCond;

    public IfBreakRegion(Region ifRegion, Region breakRegion, Edge breakEdge, 
                         Edge elseEdge, boolean invertCond) {
        this.ifRegion = ifRegion;
        this.breakRegion = breakRegion;
        this.breakEdge = breakEdge;
        this.elseEdge = elseEdge;
        this.invertCond = invertCond;
    }

    public Region getIfRegion() {
        return ifRegion;
    }

    public Region getBreakRegion() {
        return breakRegion;
    }

    public Edge getBreakEdge() {
        return breakEdge;
    }

    public Edge getElseEdge() {
        return elseEdge;
    }

    public boolean isInvertCond() {
        return invertCond;
    }
    
    public RegionType getType() {
        return RegionType.IF_BREAK;
    }

    public Region getEntryRegion() {
        return ifRegion;
    }

    public Region getExitRegionIfUnique() {
        return null;
    }

    public Collection<Region> getChildRegions() {
        return Collections.singleton(ifRegion);
    }

    public Collection<Edge> getChildEdges() {
        return Sets.newHashSet(breakEdge, elseEdge);
    }

    @Override
    public void addBreakRegion(Collection<Region> regions) {
        super.addBreakRegion(regions);
        regions.add(breakRegion);
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Region elseRegion = graph.getEdgeTarget(elseEdge);
        graph.removeEdge(elseEdge);
        graph.removeEdge(breakEdge);
        Regions.moveIncomingEdges(graph, ifRegion, this);
        graph.removeVertex(ifRegion);
        graph.addEdge(this, elseRegion, new EdgeImpl());
    }
}
