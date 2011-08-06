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

import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockType;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeAttribute;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import java.util.Collection;
import java.util.Iterator;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class IfThenElseRecognizer implements RegionRecognizer {

    public Region recognize(DirectedGraph<Region, Edge> graph, Region regionA) {
        //
        // check for if-then or if-then-else region
        //
        //     A
        //    / \
        //   B   C
        //  /     \
        // D_B    D_C
        //
        BasicBlock blockA = Regions.getDeepExitBasicBlock(graph, regionA);
        if (blockA == null || blockA.getType() != BasicBlockType.JUMP_IF) {
            return null;
        }

        if (Regions.getSuccessorCountOf(graph, regionA, false) != 2) {
            return null;
        }

        Collection<Edge> outgoingEdges = Regions.getOutgoingEdgesOf(graph, regionA, false);
        Iterator<Edge> itE = outgoingEdges.iterator();
        Edge edgeAB = itE.next();
        Edge edgeAC = itE.next();
        Region regionB = graph.getEdgeTarget(edgeAB);
        Region regionC = graph.getEdgeTarget(edgeAC);

        if (edgeAB.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)
                || edgeAC.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
            return null;
        }

        //
        // check if-return-then-return region
        //
        //         A
        //    /         \
        //   B (return)  C (return)
        //
        if (Regions.getSuccessorCountOf(graph, regionB, false) == 0
                && Regions.getSuccessorCountOf(graph, regionC, false) == 0
                && regionB.hasAttribute(RegionAttribute.RETURN)
                && regionC.hasAttribute(RegionAttribute.RETURN)) {
            return new IfThenReturnElseReturnRegion(edgeAB, edgeAC, regionA, regionB, regionC);
        }

        Region structuredRegion = null;

        //
        // check for if-then or if-then-else region
        //
        //     A
        //    / \
        //   B   C
        //  /     \
        // D_B    D_C
        //
        Edge edgeBD = Regions.getFirstOutgoingEdgeOf(graph, regionB, false);
        Edge edgeCD = Regions.getFirstOutgoingEdgeOf(graph, regionC, false);
        Region regionD_B = null;
        Region regionD_C = null;
        if (edgeBD != null) {
            regionD_B = graph.getEdgeTarget(edgeBD);
        }
        if (edgeCD != null) {
            regionD_C = graph.getEdgeTarget(edgeCD);
        }
        //
        // check if-then-else region
        //
        //     A
        //    / \
        //   B   C
        //    \ /
        //     D
        //
        if (regionD_B != null && regionD_C != null && regionD_B.equals(regionD_C)) {
            // if-then-else region
            if (Regions.getPredecessorCountOf(graph, regionB, false) == 1
                    && Regions.getPredecessorCountOf(graph, regionC, false) == 1
                    && Regions.getSuccessorCountOf(graph, regionB, false) == 1
                    && Regions.getSuccessorCountOf(graph, regionC, false) == 1) {

                if (Boolean.TRUE.equals(edgeAB.getValue())
                        && Boolean.FALSE.equals(edgeAC.getValue())) {
                    if (Regions.sameHandlers(graph, regionA, regionB, regionC)) {
                        structuredRegion = new IfThenElseJoinRegion(edgeAB, edgeBD, edgeAC, edgeCD,
                                regionA, regionB, regionC);
                    }
                } else if (Boolean.FALSE.equals(edgeAB.getValue())
                        && Boolean.TRUE.equals(edgeAC.getValue())) {
                    if (Regions.sameHandlers(graph, regionA, regionC, regionB)) {
                        structuredRegion = new IfThenElseJoinRegion(edgeAC, edgeCD, edgeAB, edgeBD,
                                regionA, regionC, regionB);
                    }
                }
            }
        } else if (regionD_C != null && regionB.equals(regionD_C)) {
            //
            // check if-then region
            //
            //     A
            //    / \
            //   |   C
            //    \ /
            //     D
            //
            if (Regions.getPredecessorCountOf(graph, regionC, false) == 1
                    && Regions.getSuccessorCountOf(graph, regionC, false) == 1) {
                if ((Boolean.TRUE.equals(edgeAB.getValue()) && Boolean.FALSE.equals(edgeAC.getValue()))
                        || (Boolean.FALSE.equals(edgeAB.getValue()) && Boolean.TRUE.equals(edgeAC.getValue()))) {
                    if (Regions.sameHandlers(graph, regionA, regionC)) {
                        boolean invertCondition = Boolean.TRUE.equals(edgeAB.getValue())
                                && Boolean.FALSE.equals(edgeAC.getValue());
                        structuredRegion = new IfThenRegion(edgeAC, edgeCD, edgeAB,
                                regionA, regionC, invertCondition);
                    }
                }
            }
        } else if (regionD_B != null && regionC.equals(regionD_B)) {
            //
            // check if-then region
            //
            //     A
            //    / \
            //   B   |
            //    \ /
            //    D_B
            //
            if (Regions.getPredecessorCountOf(graph, regionB, false) == 1
                    && Regions.getSuccessorCountOf(graph, regionB, false) == 1) {
                if ((Boolean.TRUE.equals(edgeAB.getValue()) && Boolean.FALSE.equals(edgeAC.getValue()))
                        || (Boolean.FALSE.equals(edgeAB.getValue()) && Boolean.TRUE.equals(edgeAC.getValue()))) {
                    if (Regions.sameHandlers(graph, regionA, regionB)) {
                        boolean invertCondition = Boolean.TRUE.equals(edgeAC.getValue())
                                && Boolean.FALSE.equals(edgeAB.getValue());
                        structuredRegion = new IfThenRegion(edgeAB, edgeBD, edgeAC,
                                regionA, regionB, invertCondition);
                    }
                }
            }
        }

        return structuredRegion;
    }
}
