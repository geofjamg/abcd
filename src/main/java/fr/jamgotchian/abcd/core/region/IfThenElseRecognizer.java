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
        //   |   |
        //   F   G
        //
        if (edgeAB.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE) 
                && !edgeAC.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
            boolean invertCond = Boolean.FALSE.equals(edgeAB.getValue());
            if (Regions.getSuccessorCountOf(graph, regionB, false) == 1
                    && Regions.getPredecessorCountOf(graph, regionB, false) == 1
                    && regionB.getBreakTargetStatus() != BreakTargetStatus.UNASSIGNED) {
                Edge edgeBD = Regions.getFirstOutgoingEdgeOf(graph, regionB, false);
                Region regionD = graph.getEdgeTarget(edgeBD);
                if (Regions.sameHandlers(graph, regionA, regionB)) {
                    //  A : if region
                    //  B : after then region
                    //  D : break target region
                    //  AB : then break edge
                    //  BD : after then edge
                    //  AC : else edge
                    return IfThenBreakRegion.newInstance2(regionA, regionD, edgeAC,
                                                          edgeAB, regionB, edgeBD,
                                                          invertCond);
                }
            } else {
                //  A : if region
                //  AB : then break edge
                //  AC : else edge
                return IfThenBreakRegion.newInstance(regionA, regionB, edgeAC, edgeAB,
                                                     invertCond);
            }
        }

        if (!edgeAB.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE) 
                && edgeAC.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
            boolean invertCond = Boolean.FALSE.equals(edgeAC.getValue());
            if (Regions.getSuccessorCountOf(graph, regionC, false) == 1
                    && Regions.getPredecessorCountOf(graph, regionC, false) == 1
                    && regionC.getBreakTargetStatus() != BreakTargetStatus.UNASSIGNED) {
                //  A : if region
                //  C : after then region
                //  E : break target region
                //  AC : then break edge
                //  CE : after then edge
                //  AB : else edge
                Edge edgeCE = Regions.getFirstOutgoingEdgeOf(graph, regionC, false);
                Region regionE = graph.getEdgeTarget(edgeCE);
                if (Regions.sameHandlers(graph, regionA, regionC)) {
                    return IfThenBreakRegion.newInstance2(regionA, regionE, edgeAB,
                                                          edgeAC, regionC, edgeCE,
                                                          invertCond);
                }
            } else {
                //  A : if region
                //  AC : then break edge
                //  AB : else edge
                return IfThenBreakRegion.newInstance(regionA, regionC, edgeAB, edgeAC,
                                                     invertCond);
            }
        }

        //  A : if region
        //  B : before then region
        //  D : after then region
        //  AB : before then edge
        //  BD : then break edge
        //  DF : after then edge
        //  AC : else edge
        if (Regions.getSuccessorCountOf(graph, regionB, false) == 1
                && Regions.sameHandlers(graph, regionA, regionB)) {
            Edge edgeBD = Regions.getFirstOutgoingEdgeOf(graph, regionB, false);
            if (edgeBD.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                Region regionD = graph.getEdgeTarget(edgeBD);
                if (Regions.getSuccessorCountOf(graph, regionD, false) == 1) {
                    Edge edgeDF = Regions.getFirstOutgoingEdgeOf(graph, regionD, false);
                    Region regionF = graph.getEdgeTarget(edgeDF);
                    boolean invertCond = Boolean.FALSE.equals(edgeAB.getValue());
                    return IfThenBreakRegion.newInstance3(regionA, regionF, edgeAC,
                                                          edgeBD, regionB, edgeAB,
                                                          regionD, edgeDF, invertCond);
                }
            }
        }

        //  A : if region
        //  C : before then region
        //  E : before then region
        //  AC : before then edge
        //  CE : then break edge
        //  EG : after then edge
        //  AB : else edge
        if (Regions.getSuccessorCountOf(graph, regionC, false) == 1
                && Regions.sameHandlers(graph, regionA, regionC)) {
            Edge edgeCE = Regions.getFirstOutgoingEdgeOf(graph, regionC, false);
            if (edgeCE.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                boolean invertCond = Boolean.FALSE.equals(edgeAC.getValue());
                Region regionE = graph.getEdgeTarget(edgeCE);
                if (Regions.getSuccessorCountOf(graph, regionE, false) == 1) {
                    Edge edgeEG = Regions.getFirstOutgoingEdgeOf(graph, regionE, false);
                    Region regionG = graph.getEdgeTarget(edgeEG);
                    return IfThenBreakRegion.newInstance3(regionA, regionG, edgeAB,
                                                          edgeCE, regionC, edgeAC,
                                                          regionE, edgeEG, invertCond);
                }
            }
        }

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
        if (!edgeAB.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE) 
                && !edgeAC.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
            structuredRegion = recognizeIfThenElse(graph, regionA, regionB, regionC, edgeAB, edgeAC);
        }
        if (structuredRegion == null) {
            structuredRegion = recognizeIfThenBreak(graph, regionA, regionB, regionC, edgeAB, edgeAC);
        }
        return structuredRegion;
    }
}
