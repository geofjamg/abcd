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
import fr.jamgotchian.abcd.core.controlflow.EdgeCategory;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class LoopRecognizer implements RegionRecognizer {

    public Region recognize(DirectedGraph<Region, Edge> graph, Region loopHeadRegion) {
        Region structuredRegion = null;
        if (Regions.getPredecessorCountOf(graph, loopHeadRegion, false) == 2
                && (Regions.getSuccessorCountOf(graph, loopHeadRegion, false) == 1
                || Regions.getSuccessorCountOf(graph, loopHeadRegion, false) == 2)) {

            Iterator<Edge> it = Regions.getIncomingEdgesOf(graph, loopHeadRegion, false).iterator();
            Edge incomingEdge1 = it.next();
            Edge incomingEdge2 = it.next();
            it = Regions.getOutgoingEdgesOf(graph, loopHeadRegion, false).iterator();
            Edge outgoingEdge1 = it.next();
            Edge outgoingEdge2 = null;
            if (it.hasNext()) {
                outgoingEdge2 = it.next();
            }

            //
            // check for a loop region
            //
            //     ...
            //      |
            //      X<--
            //    / |   |
            //      X   |
            //    / |   |
            //     ...
            //      |   |
            //      X---
            //      |
            //
            Edge loopBackEdge = null;
            List<LoopSubRegion> subRegions = null;
            if (incomingEdge1.equals(outgoingEdge1) || incomingEdge1.equals(outgoingEdge2)) {
                loopBackEdge = incomingEdge1;
                subRegions = Collections.emptyList();
            } else if (incomingEdge2.equals(outgoingEdge1) || incomingEdge2.equals(outgoingEdge2)) {
                loopBackEdge = incomingEdge2;
                subRegions = Collections.emptyList();
            } else if (incomingEdge1.isLoopBack()) {
                loopBackEdge = incomingEdge1;
            } else if (incomingEdge2.isLoopBack()) {
                loopBackEdge = incomingEdge2;
            }

            if (loopBackEdge != null) {
                Region loopTailRegion = graph.getEdgeSource(loopBackEdge);

                if (Regions.getPredecessorCountOf(graph, loopTailRegion, false) == 1) {

                    subRegions = new ArrayList<LoopSubRegion>();

                    Region loopRegion = null;
                    for (loopRegion = Regions.getFirstPredecessorOf(graph, loopTailRegion, false);
                            !loopRegion.equals(loopHeadRegion);
                            loopRegion = Regions.getFirstPredecessorOf(graph, loopRegion, false)) {

                        if (Regions.getPredecessorCountOf(graph, loopRegion, false) == 1
                                && Regions.getSuccessorCountOf(graph, loopRegion, false) == 2) {
                            Iterator<Edge> itE = Regions.getOutgoingEdgesOf(graph, loopRegion, false).iterator();
                            Edge maybeExitEdge1 = itE.next();
                            Edge maybeExitEdge2 = itE.next();

                            Edge loopExitEdge = null;
                            Edge loopEdge = null;
                            if (maybeExitEdge1.isLoopExit() && !maybeExitEdge2.isLoopExit()) {
                                loopExitEdge = maybeExitEdge1;
                                loopEdge = maybeExitEdge2;
                            } else if (maybeExitEdge2.isLoopExit() && !maybeExitEdge1.isLoopExit()) {
                                loopExitEdge = maybeExitEdge2;
                                loopEdge = maybeExitEdge1;
                            }

                            if (loopExitEdge == null || loopEdge == null) {
                                subRegions = null;
                                break;
                            } else {
                                subRegions.add(0, new LoopSubRegion(loopRegion, loopEdge, loopExitEdge));
                            }
                        }
                    }

                    if (loopRegion.equals(loopHeadRegion)) {
                        Iterator<Edge> itE = Regions.getOutgoingEdgesOf(graph, loopRegion, false).iterator();
                        Edge maybeExitEdge1 = itE.next();
                        Edge maybeExitEdge2 = itE.next();

                        Edge loopExitEdge = null;
                        Edge loopEdge = null;
                        if (maybeExitEdge1.isLoopExit() && !maybeExitEdge2.isLoopExit()) {
                            loopExitEdge = maybeExitEdge1;
                            loopEdge = maybeExitEdge2;
                        } else if (maybeExitEdge2.isLoopExit() && !maybeExitEdge1.isLoopExit()) {
                            loopExitEdge = maybeExitEdge2;
                            loopEdge = maybeExitEdge1;
                        }

                        if (loopExitEdge != null && loopEdge != null) {
                            subRegions.add(0, new LoopSubRegion(loopRegion, loopEdge, loopExitEdge));
                        }
                    }
                }

                if (subRegions != null) {
                    LoopType loopType = Regions.getSuccessorCountOf(graph, loopTailRegion, false) == 1
                            ? LoopType.WHILE : LoopType.DO_WHILE;
                    structuredRegion = new LoopRegion(loopType, loopBackEdge, loopTailRegion, subRegions);
                }
            }
        }

        return structuredRegion;
    }
}
