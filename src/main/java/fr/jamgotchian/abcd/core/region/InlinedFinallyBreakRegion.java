/*
 * Copyright (C) 2011 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *  *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package fr.jamgotchian.abcd.core.region;

import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class InlinedFinallyBreakRegion extends AbstractRegion {

    private final Region tryRegion;

    private final Edge tryEdge;

    private final Region finallyRegion;

    public InlinedFinallyBreakRegion(Region tryRegion, Edge tryEdge, Region finallyRegion) {
        this.tryRegion = tryRegion;
        this.tryEdge = tryEdge;
        this.finallyRegion = finallyRegion;
    }

    public RegionType getType() {
        return RegionType.INLINED_FINALLY_BREAK;
    }

    public Region getEntryRegion() {
        return tryRegion;
    }

    public Region getExitRegion() {
        return tryRegion;
    }

    public Region getTryRegion() {
        return tryRegion;
    }

    public List<Region> getChildRegions() {
        return Arrays.asList(tryRegion, finallyRegion);
    }

    public Collection<Edge> getChildEdges() {
        return Collections.singleton(tryEdge);
    }

    public void reduce(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveHandlers(graph, tryRegion, this);
        Regions.moveIncomingEdges(graph, tryRegion, this);
        graph.removeEdge(tryEdge);
        graph.removeVertex(tryRegion);
        graph.removeVertex(finallyRegion);
        setBreak(true);
    }
}
