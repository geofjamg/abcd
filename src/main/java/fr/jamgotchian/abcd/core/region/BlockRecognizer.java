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
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class BlockRecognizer implements RegionRecognizer {

    private BlockRegion checkForward(DirectedGraph<Region, Edge> graph, Region regionA) {
        if (Regions.getSuccessorCountOf(graph, regionA, false) != 1) {
            return null;
        }
        List<Region> internalRegions = new ArrayList<Region>();
        Region r = null;
        for (r = regionA; isBlockForward(graph, r); r = Regions.getFirstSuccessorOf(graph, r, false)) {
            internalRegions.add(r);
        }
        internalRegions.add(r);
        if (internalRegions.size() > 1) {
            Set<Edge> internalEdges = new HashSet<Edge>();
            for (int i = 0; i < internalRegions.size() - 1; i++) {
                internalEdges.add(graph.getEdge(internalRegions.get(i), internalRegions.get(i + 1)));
            }
            return new BlockRegion(internalEdges, internalRegions);
        }
        return null;
    }

    private BlockRegion checkBackward(DirectedGraph<Region, Edge> graph, Region regionA) {
        if (Regions.getPredecessorCountOf(graph, regionA, false) != 1) {
            return null;
        }
        List<Region> internalRegions = new ArrayList<Region>();
        Region r = null;
        for (r = regionA; isBlockBackward(graph, r); r = Regions.getFirstPredecessorOf(graph, r, false)) {
            internalRegions.add(0, r);
        }
        internalRegions.add(0, r);
        if (internalRegions.size() > 1) {
            Set<Edge> internalEdges = new HashSet<Edge>();
            for (int i = 0; i < internalRegions.size() - 1; i++) {
                internalEdges.add(graph.getEdge(internalRegions.get(i), internalRegions.get(i + 1)));
            }
            return new BlockRegion(internalEdges, internalRegions);
        }
        return null;
    }
    
    private boolean isBlockForward(DirectedGraph<Region, Edge> graph, Region regionA) {
        if (Regions.getSuccessorCountOf(graph, regionA, false) != 1) {
            return false;
        }
        Region regionB = Regions.getFirstSuccessorOf(graph, regionA, false);
        if (Regions.getPredecessorCountOf(graph, regionB, false) != 1) {
            return false;
        }
        return Regions.sameHandlers(graph, regionA, regionB);
    }

    private boolean isBlockBackward(DirectedGraph<Region, Edge> graph, Region regionA) {
        if (Regions.getPredecessorCountOf(graph, regionA, false) != 1) {
            return false;
        }
        Region regionB = Regions.getFirstPredecessorOf(graph, regionA, false);
        if (Regions.getSuccessorCountOf(graph, regionB, false) != 1) {
            return false;
        }
        return Regions.sameHandlers(graph, regionA, regionB);
    }

    public Region recognize(DirectedGraph<Region, Edge> graph, Region regionA) {
        //
        // check for block region forward
        //
        //  ...    incomingExternalEdge
        //   A    regionA
        //   |
        //  ...             internalRegions = regionA ... regionZ
        //   |
        //   Z    region Z
        //  ...    outgoingExternalEdges
        //
        Region structuredRegion = checkForward(graph, regionA);

        //
        // check for block region backward
        //
        //  ...    incomingExternalEdge
        //   Z    regionZ
        //   |
        //  ...             internalRegions = regionZ ... regionA
        //   |
        //   A    region A
        //  ...    outgoingExternalEdges
        //
        if (structuredRegion == null) {
            structuredRegion = checkBackward(graph, regionA);
        }

        return structuredRegion;
    }
}
