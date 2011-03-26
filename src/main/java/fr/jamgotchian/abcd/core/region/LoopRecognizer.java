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
import java.util.Iterator;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class LoopRecognizer implements RegionRecognizer {

    public Region recognize(DirectedGraph<Region, Edge> graph, Region loopRegion) {
        if (Regions.getPredecessorCountOf(graph, loopRegion, false) == 2) {
            Iterator<Edge> it = Regions.getIncomingEdgesOf(graph, loopRegion, false).iterator();
            Edge incomingEdge1 = it.next();
            Edge incomingEdge2 = it.next();
            Region loopHeadRegion = loopRegion.getEntryRegion();
            Region loopTailRegion = loopRegion.getExitRegionIfUnique();
            //
            // check for a while loop region
            //
            //     ...
            //      |
            //      |<--
            //      X   |
            //       ---
            //
            if (loopHeadRegion != null && loopHeadRegion.getType() == RegionType.IF_BREAK
                    && Regions.getSuccessorCountOf(graph, loopRegion, false) == 1) {
                Edge outgoingEdge = Regions.getFirstOutgoingEdgeOf(graph, loopRegion, false);
                if (outgoingEdge.isLoopBack()
                        && (outgoingEdge.equals(incomingEdge1) || outgoingEdge.equals(incomingEdge2))) {
                    return new LoopRegion(LoopType.WHILE, outgoingEdge, loopRegion);
                } else {
                    return null;
                }
            }
            //
            // check for a do while loop region
            //
            //     ...
            //      |
            //      |<--
            //      X   |
            //      |---
            //      |
            //     ...
            //
            else if (loopTailRegion != null && loopTailRegion.getType() == RegionType.IF_THEN_ELSE
                    && Regions.getSuccessorCountOf(graph, loopRegion, false) == 2) {
                it = Regions.getOutgoingEdgesOf(graph, loopRegion, false).iterator();
                Edge outgoingEdge1 = it.next();
                Edge outgoingEdge2 = it.next();
                if (outgoingEdge1.isLoopBack()
                        && (outgoingEdge1.equals(incomingEdge1) || outgoingEdge1.equals(incomingEdge2))) {
                    return new LoopRegion(LoopType.DO_WHILE, outgoingEdge1, loopRegion);
                } else if (outgoingEdge2.isLoopBack()
                        && (outgoingEdge2.equals(incomingEdge1) || outgoingEdge2.equals(incomingEdge2))) {
                    return new LoopRegion(LoopType.DO_WHILE, outgoingEdge2, loopRegion);
                } else {
                    return null;
                }
            } else {
                return null;
            }
        }
        return null;
    }
}
