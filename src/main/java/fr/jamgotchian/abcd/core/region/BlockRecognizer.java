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

    public Region recognize(DirectedGraph<Region, Edge> graph, Region regionA) {
        Region structuredRegion = null;
        //
        // check for block region
        //
        //  ...    incomingExternalEdge
        //   A    regionA
        //   |
        //  ...             internalRegions = regionA ... regionZ
        //   |
        //   Z    region Z
        //  ...    outgoingExternalEdges
        //
        if (graph.getSuccessorCountOf(regionA) == 1) {
            List<Region> internalRegions = new ArrayList<Region>();
            Region r = null;
            for (r = regionA;
                    graph.getSuccessorCountOf(r) == 1
                    && graph.getPredecessorCountOf(graph.getFirstSuccessorsOf(r)) == 1;
                    r = graph.getFirstSuccessorsOf(r)) {
                internalRegions.add(r);
            }
            internalRegions.add(r);
            if (internalRegions.size() > 1) {
                Set<Edge> internalEdges = new HashSet<Edge>();
                for (int i = 0; i < internalRegions.size() - 1; i++) {
                    internalEdges.add(graph.getEdge(internalRegions.get(i), internalRegions.get(i + 1)));
                }
                structuredRegion = new BlockRegion(internalEdges, internalRegions);
            }
        }
        return structuredRegion;
    }
}
