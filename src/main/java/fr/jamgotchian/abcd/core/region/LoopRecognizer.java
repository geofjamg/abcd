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
import fr.jamgotchian.abcd.core.controlflow.BasicBlockAnalysisData;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import java.util.Iterator;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class LoopRecognizer implements RegionRecognizer {

    private int currentLoopID = 0;
    
    public Region recognize(DirectedGraph<Region, Edge> graph, Region loopRegion) {
        if (Regions.getPredecessorCountOf(graph, loopRegion, false) == 2) {
            Iterator<Edge> it = Regions.getIncomingEdgesOf(graph, loopRegion, false).iterator();
            Edge incomingEdge1 = it.next();
            Edge incomingEdge2 = it.next();
            
            //
            // check for a loop region
            //
            //     ...
            //      |
            //      |<--
            //      A   |
            //      |---
            //
            if (Regions.getSuccessorCountOf(graph, loopRegion, false) == 1) {
                Edge outgoingEdge = Regions.getFirstOutgoingEdgeOf(graph, loopRegion, false);
                if (outgoingEdge.isLoopBack()
                        && (outgoingEdge.equals(incomingEdge1) || outgoingEdge.equals(incomingEdge2))) {
                    LoopType type = LoopType.INFINITE;
                    Region loopTailRegion = Regions.getDeepExitRegion(graph, loopRegion);
                    Region loopHeadRegion = Regions.getDeepEntryRegion(graph, loopRegion);
                    if (loopRegion.getType() == RegionType.IF_THEN_BREAK
                            || (loopTailRegion != null && loopTailRegion.getType() == RegionType.IF_THEN_BREAK)) {
                        type = LoopType.DO_WHILE;
                    } else if (loopHeadRegion != null && loopHeadRegion.getType() == RegionType.IF_THEN_BREAK) {
                        BasicBlock loopHeadBlock = Regions.getDeepExitBasicBlock(graph, loopHeadRegion);
                        if (loopHeadBlock != null) {
                            BasicBlockAnalysisData data = (BasicBlockAnalysisData) loopHeadBlock.getData();
                            if (data.getStatementCount() == 1) {
                                type = LoopType.WHILE;
                            }
                        }
                    }
                    return new LoopRegion(type, outgoingEdge, loopRegion, currentLoopID++);
                }
            }
        }
        return null;
    }
}
