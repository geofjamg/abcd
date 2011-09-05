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
import fr.jamgotchian.abcd.core.controlflow.EdgeAttribute;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class BlockRecognizer implements RegionRecognizer {

    BlockRecognizer() {
    }

    private BlockRegion checkForward(RegionGraph graph, Region regionA) {
        if (graph.getSuccessorCountOf(regionA, false) != 1) {
            return null;
        }
        List<Region> internalRegions = new ArrayList<Region>();
        Region r = null;
        for (r = regionA; isBlockForward(graph, r); r = graph.getFirstSuccessorOf(r, false)) {
            internalRegions.add(r);
        }
        internalRegions.add(r);
        if (internalRegions.size() > 1) {
            List<Edge> internalEdges = new ArrayList<Edge>();
            for (int i = 0; i < internalRegions.size() - 1; i++) {
                internalEdges.add(graph.getEdge(internalRegions.get(i), internalRegions.get(i + 1)));
            }
            return new BlockRegion(internalEdges, internalRegions);
        }
        return null;
    }

    private BlockRegion checkBackward(RegionGraph graph, Region regionA) {
        if (graph.getPredecessorCountOf(regionA, false) != 1) {
            return null;
        }
        List<Region> internalRegions = new ArrayList<Region>();
        Region r = null;
        for (r = regionA; isBlockBackward(graph, r); r = graph.getFirstPredecessorOf(r, false)) {
            internalRegions.add(0, r);
        }
        internalRegions.add(0, r);
        if (internalRegions.size() > 1) {
        List<Edge> internalEdges = new ArrayList<Edge>();
            for (int i = 0; i < internalRegions.size() - 1; i++) {
                internalEdges.add(graph.getEdge(internalRegions.get(i), internalRegions.get(i + 1)));
            }
            return new BlockRegion(internalEdges, internalRegions);
        }
        return null;
    }

    private boolean isBlockForward(RegionGraph graph, Region regionA) {
        if (graph.getSuccessorCountOf(regionA, false) != 1) {
            return false;
        }
        Edge edgeAB = graph.getFirstOutgoingEdgeOf(regionA, false);
        if (edgeAB.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
            return false;
        }
        Region regionB = graph.getEdgeTarget(edgeAB);
        if (graph.getPredecessorCountOf(regionB, false) != 1) {
            return false;
        }
        return Regions.sameHandlers(graph, regionA, regionB);
    }

    private boolean isBlockBackward(RegionGraph graph, Region regionA) {
        if (graph.getPredecessorCountOf(regionA, false) != 1) {
            return false;
        }
        Edge edgeBA = graph.getFirstIncomingEdgeOf(regionA, false);
        Region regionB = graph.getEdgeSource(edgeBA);
        if (graph.getSuccessorCountOf(regionB, false) != 1) {
            return false;
        }
        return Regions.sameHandlers(graph, regionA, regionB);
    }

    public Region recognize(RegionGraph graph, Region regionA) {
        //
        // check for block region forward
        //
        //   A
        //   |
        //  ...
        //   |
        //   Z
        //
        Region structuredRegion = checkForward(graph, regionA);

        //
        // check for block region backward
        //
        //   Z
        //   |
        //  ...
        //   |
        //   A
        //
        if (structuredRegion == null) {
            structuredRegion = checkBackward(graph, regionA);
        }

        return structuredRegion;
    }
}
