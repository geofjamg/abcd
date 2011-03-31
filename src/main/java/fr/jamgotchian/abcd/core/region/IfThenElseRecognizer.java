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
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import java.util.Collection;
import java.util.Iterator;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class IfThenElseRecognizer implements RegionRecognizer {

    private Region recognizeIfThenElse(DirectedGraph<Region, Edge> graph, Region regionA,
                                       Region regionB, Region regionC, Edge edgeAB, Edge edgeAC) {

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
                        structuredRegion = new IfThenElseRegion(edgeAB, edgeBD, edgeAC, edgeCD,
                                regionA, regionB, regionC);
                    }
                } else if (Boolean.FALSE.equals(edgeAB.getValue())
                        && Boolean.TRUE.equals(edgeAC.getValue())) {
                    if (Regions.sameHandlers(graph, regionA, regionC, regionB)) {
                        structuredRegion = new IfThenElseRegion(edgeAC, edgeCD, edgeAB, edgeBD,
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

    private Region recognizeIfThenBreak(DirectedGraph<Region, Edge> graph, Region regionA,
                                        Region regionB, Region regionC, Edge edgeAB, Edge edgeAC) {
        //
        // check if-then-break region
        //
        //     A
        //    / \
        //   B   C
        //   |   |
        //   D   E
        //
        if (edgeAB.isLoopExit() && !edgeAC.isLoopExit()) {
            boolean invertCond = Boolean.FALSE.equals(edgeAB.getValue());
            if (Regions.getSuccessorCountOf(graph, regionB, false) == 1
                    && Regions.getPredecessorCountOf(graph, regionB, false) == 1
                    && !regionB.isBreakTarget()) {
                Edge edgeBD = Regions.getFirstOutgoingEdgeOf(graph, regionB, false);
                Region regionD = graph.getEdgeTarget(edgeBD);
                if (Regions.sameHandlers(graph, regionA, regionB)) {
                    //  A : if region
                    //  B : then region
                    //  D : break target region
                    //  AB : then edge => loop exit
                    //  BD : then edge 2
                    //  AC : else edge
                    return new IfThenBreakRegion(regionA, regionD, edgeAC,
                                                 edgeAB, regionB, edgeBD,
                                                 invertCond);
                }
            } else {
                //  A : if region
                //  AB : then edge => loop exit
                //  AC : else edge
                return new IfThenBreakRegion(regionA, regionB, edgeAC, edgeAB,
                                             null, null, invertCond);
            }
        }

        if (!edgeAB.isLoopExit() && edgeAC.isLoopExit()) {
            boolean invertCond = Boolean.FALSE.equals(edgeAC.getValue());
            if (Regions.getSuccessorCountOf(graph, regionC, false) == 1
                    && Regions.getPredecessorCountOf(graph, regionC, false) == 1
                    && !regionC.isBreakTarget()) {
                //  A : if region
                //  C : then region
                //  E : break target region
                //  AC : then edge => loop exit
                //  CE : then edge 2
                //  AB : else edge
                Edge edgeCE = Regions.getFirstOutgoingEdgeOf(graph, regionC, false);
                Region regionE = graph.getEdgeTarget(edgeCE);
                if (Regions.sameHandlers(graph, regionA, regionC)) {
                    return new IfThenBreakRegion(regionA, regionE, edgeAB,
                                                 edgeAC, regionC, edgeCE,
                                                 invertCond);
                }
            } else {
                //  A : if region
                //  AC : then edge => loop exit
                //  AB : else edge
                return new IfThenBreakRegion(regionA, regionC, edgeAB, edgeAC,
                                             null, null, invertCond);
            }
        }

        //  A : if region
        //  B : then region
        //  AB : then edge
        //  BD : then edge 2 => loop exit
        //  AC : else edge
//        if (Regions.getSuccessorCountOf(graph, regionB, false) == 1
//                && Regions.sameHandlers(graph, regionA, regionB)) {
//            Edge edgeBD = Regions.getFirstOutgoingEdgeOf(graph, regionB, false);
//            if (edgeBD.isLoopExit()) {
//                boolean invertCond = Boolean.FALSE.equals(edgeAB.getValue());
//                Region regionD = graph.getEdgeTarget(edgeBD);
//                return new IfThenBreakRegion(regionA, regionD, edgeAC,
//                                             edgeAB, regionB, edgeBD,
//                                             invertCond);
//            }
//        }

        //  A : if region
        //  C : then region
        //  AC : then edge
        //  CE : then edge 2 => loop exit
        //  AB : else edge
//        if (Regions.getSuccessorCountOf(graph, regionC, false) == 1
//                && Regions.sameHandlers(graph, regionA, regionC)) {
//            Edge edgeCE = Regions.getFirstOutgoingEdgeOf(graph, regionC, false);
//            if (edgeCE.isLoopExit()) {
//                boolean invertCond = Boolean.FALSE.equals(edgeAC.getValue());
//                Region regionE = graph.getEdgeTarget(edgeCE);
//                return new IfThenBreakRegion(regionA, regionE, edgeAB,
//                                             edgeAC, regionC, edgeCE,
//                                             invertCond);
//            }
//        }

        return null;
    }

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

        Region structuredRegion = null;
        if (!edgeAB.isLoopExit() && !edgeAC.isLoopExit()) {
            structuredRegion = recognizeIfThenElse(graph, regionA, regionB, regionC, edgeAB, edgeAC);
        }
        if (structuredRegion == null) {
            structuredRegion = recognizeIfThenBreak(graph, regionA, regionB, regionC, edgeAB, edgeAC);
        }
        return structuredRegion;
    }
}
