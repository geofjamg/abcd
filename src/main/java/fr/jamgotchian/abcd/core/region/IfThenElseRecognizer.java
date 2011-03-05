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
import java.util.Collection;
import java.util.Iterator;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class IfThenElseRecognizer implements RegionRecognizer {

    public Region recognize(DirectedGraph<Region, Edge> graph, Region regionA) {
        Region structuredRegion = null;
        //
        // check for if-then or if-then-else region
        //
        //    ...      incomingExternalEdge
        //     A       region A
        //    / \
        //   B   C     region B and C
        //  /     \
        // D_B    D_C  region D_B and D_C
        //    ...      outgoingExternalEdges
        //
        if (Regions.getSuccessorCountOf(graph, regionA, false) == 2) {
            Collection<Edge> outgoingEdges = Regions.getOutgoingEdgesOf(graph, regionA, false);
            Iterator<Edge> itE = outgoingEdges.iterator();
            Edge edgeAB = itE.next();
            Edge edgeAC = itE.next();
            if (!edgeAB.isLoopExit() && !edgeAC.isLoopExit()) {
                Collection<Region> successors = Regions.getSuccessorsOf(graph, regionA, false);
                Iterator<Region> itR = successors.iterator();
                Region regionB = itR.next();
                Region regionC = itR.next();

                Region regionD_B = null;
                Region regionD_C = null;
                itR = Regions.getSuccessorsOf(graph, regionB, false).iterator();
                if (itR.hasNext()) {
                    regionD_B = itR.next();
                }
                itR = Regions.getSuccessorsOf(graph, regionC, false).iterator();
                if (itR.hasNext()) {
                    regionD_C = itR.next();
                }
                //
                // check if-then-else region
                //
                //    ...    incomingExternalEdge
                //     A     region A
                //    / \
                //   B   C   region B and C
                //    \ /
                //     D     region D
                //    ...    outgoingExternalEdges
                //
                Edge edgeBD = Regions.getFirstOutgoingEdgeOf(graph, regionB, false);
                Edge edgeCD = Regions.getFirstOutgoingEdgeOf(graph, regionC, false);
                if (regionD_B != null && regionD_C != null && regionD_B.equals(regionD_C)) {
                    // if-then-else region
                    if (Regions.getPredecessorCountOf(graph, regionB, false) == 1
                            && Regions.getPredecessorCountOf(graph, regionC, false) == 1
                            && Regions.getSuccessorCountOf(graph, regionB, false) == 1
                            && Regions.getSuccessorCountOf(graph, regionC, false) == 1) {

                        if (Boolean.TRUE.equals(edgeAB.getValue()) 
                                && Boolean.FALSE.equals(edgeAC.getValue())) {
                            if (Regions.sameHandlers(graph, regionA, regionB, regionC)) {
                                structuredRegion = new IfThenElseRegion(edgeAB, edgeAC, edgeBD, edgeCD,
                                        regionA, regionB, regionC);
                            }
                        } else if (Boolean.FALSE.equals(edgeAB.getValue()) 
                                && Boolean.TRUE.equals(edgeAC.getValue())) {
                            if (Regions.sameHandlers(graph, regionA, regionC, regionB)) {
                                structuredRegion = new IfThenElseRegion(edgeAC, edgeAB, edgeCD, edgeBD,
                                        regionA, regionC, regionB);
                            }
                        }
                    }
                } else if (regionD_C != null && regionB.equals(regionD_C)) {
                    //
                    // check if-then region
                    //
                    //    ...    incomingExternalEdge
                    //     A     region A
                    //    / \
                    //   |   C   region C
                    //    \ /
                    //     D     region D
                    //    ...    outgoingExternalEdges
                    //
                    if (Regions.getPredecessorCountOf(graph, regionC, false) == 1
                            && Regions.getSuccessorCountOf(graph, regionC, false) == 1) {
                        if ((Boolean.TRUE.equals(edgeAB.getValue()) && Boolean.FALSE.equals(edgeAC.getValue()))
                                || (Boolean.FALSE.equals(edgeAB.getValue()) && Boolean.TRUE.equals(edgeAC.getValue()))) {
                            if (Regions.sameHandlers(graph, regionA, regionC)) {
                                boolean invertCondition = Boolean.TRUE.equals(edgeAB.getValue())
                                        && Boolean.FALSE.equals(edgeAC.getValue());
                                structuredRegion = new IfThenRegion(edgeAB, edgeAC, edgeCD,
                                        regionA, regionC, invertCondition);
                            }
                        }
                    }
                } else if (regionD_B != null && regionC.equals(regionD_B)) {
                    //
                    // check if-then region
                    //
                    //    ...    incomingExternalEdge
                    //     A     region A
                    //    / \
                    //   B   |   region B
                    //    \ /
                    //    D_B    region D
                    //    ...    outgoingExternalEdges
                    //
                    if (Regions.getPredecessorCountOf(graph, regionB, false) == 1
                            && Regions.getSuccessorCountOf(graph, regionB, false) == 1) {
                        if ((Boolean.TRUE.equals(edgeAB.getValue()) && Boolean.FALSE.equals(edgeAC.getValue()))
                                || (Boolean.FALSE.equals(edgeAB.getValue()) && Boolean.TRUE.equals(edgeAC.getValue()))) {
                            if (Regions.sameHandlers(graph, regionA, regionB)) {                            
                                boolean invertCondition = Boolean.TRUE.equals(edgeAC.getValue())
                                        && Boolean.FALSE.equals(edgeAB.getValue());
                                structuredRegion = new IfThenRegion(edgeAB, edgeAC, edgeBD,
                                        regionA, regionB, invertCondition);
                            }
                        }
                    }
                }
            }
        }
        
        return structuredRegion;
    }
}
