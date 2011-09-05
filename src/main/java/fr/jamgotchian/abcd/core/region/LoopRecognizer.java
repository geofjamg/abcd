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
import java.util.Collection;
import java.util.Iterator;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class LoopRecognizer implements RegionRecognizer {

    public Region recognize(RegionGraph graph, Region regionA) {
        if (graph.getPredecessorCountOf(regionA, false) != 2) {
            return null;
        }
        Iterator<Edge> itI = graph.getIncomingEdgesOf(regionA, false).iterator();
        Edge incomingEdge1 = itI.next();
        Edge incomingEdge2 = itI.next();

        Edge loopBackEdge = null;
        Edge loopEntryEdge = null;
        if (incomingEdge1.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
            loopBackEdge = incomingEdge1;
            loopEntryEdge = incomingEdge2;
        }
        if (incomingEdge2.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
            loopBackEdge = incomingEdge2;
            loopEntryEdge = incomingEdge1;
        }
        if (loopBackEdge == null || loopEntryEdge == null) {
            return null;
        }

        //
        // check for a infinite loop region
        //
        //     ...
        //      |
        //      |<--+
        //      A   |
        //      +---+
        //
        if (graph.getSuccessorCountOf(regionA, false) == 1) {
            Edge outgoingEdge = graph.getFirstOutgoingEdgeOf(regionA, false);
            if (outgoingEdge.equals(loopBackEdge)) {
                return new InfiniteLoopRegion(outgoingEdge, regionA);
            }
        }

        if (graph.getSuccessorCountOf(regionA, false) == 2) {
            //
            // check for a do while loop region
            //
            //     ...
            //      |
            //      |<--+
            //      A   | t/f
            //      +---+
            //      |
            //      | t/f
            //
            Collection<Edge> outgoingEdges = graph.getOutgoingEdgesOf(regionA, false);
            Iterator<Edge> itO = outgoingEdges.iterator();
            Edge outgoingEdge1 = itO.next();
            Edge outgoingEdge2 = itO.next();
            if (!Boolean.TRUE.equals(outgoingEdge1.getValue())
                    && !Boolean.FALSE.equals(outgoingEdge1.getValue())) {
                return null;
            }
            if (!Boolean.TRUE.equals(outgoingEdge2.getValue())
                    && !Boolean.FALSE.equals(outgoingEdge2.getValue())) {
                return null;
            }
            if (outgoingEdge1.equals(loopBackEdge)) {
                return new DoWhileLoopRegion(outgoingEdge1, regionA, outgoingEdge2);
            }
            if (outgoingEdge2.equals(loopBackEdge)) {
                return new DoWhileLoopRegion(outgoingEdge2, regionA, outgoingEdge1);
            }

            //
            // check for a while loop region
            //
            //        ...
            //         |
            //         |<-----+
            //  +------A      |
            //  | t/f  | t/f  |
            //  C      B      |
            //         |      |
            //         +------+
            //
            Edge edgeAB = null;
            Edge edgeAC = null;
            if (outgoingEdge1.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                edgeAB = outgoingEdge2;
                edgeAC = outgoingEdge1;
            } else if (outgoingEdge2.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                edgeAB = outgoingEdge1;
                edgeAC = outgoingEdge2;
            }
            if (edgeAB == null || edgeAC == null) {
                return null;
            }
            if (!Boolean.TRUE.equals(edgeAB.getValue())
                    && !Boolean.FALSE.equals(edgeAB.getValue())) {
                return null;
            }
            if (!Boolean.TRUE.equals(edgeAC.getValue())
                    && !Boolean.FALSE.equals(edgeAC.getValue())) {
                return null;
            }
            Region regionB = graph.getEdgeTarget(edgeAB);
            Region regionC = graph.getEdgeTarget(edgeAC);
            if (graph.getPredecessorCountOf(regionB) != 1
                    || graph.getPredecessorCountOf(regionC) != 1) {
                return null;
            }
            if (!Regions.sameHandlers(graph, regionA, regionB/*, regionC*/)) {
                return null;
            }
            if (graph.getSuccessorCountOf(regionB, false) != 1) {
                return null;
            }
            Edge edgeBX = graph.getFirstOutgoingEdgeOf(regionB, false);
            if (!edgeBX.equals(loopBackEdge)) {
                return null;
            }
            return new WhileLoopRegion(loopBackEdge, regionA, edgeAC, regionC, edgeAB, regionB);
        }

        return null;
    }
}
