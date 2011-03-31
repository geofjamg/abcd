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
public class LogicalRecognizer implements RegionRecognizer {
    
    private Region recognizeAnd(DirectedGraph<Region, Edge> graph, Region regionA,
                                Edge trueEdgeA, Edge falseEdgeA) {
        //
        // check for AND
        //
        //     A
        //   t/ \f
        //   B   \
        // t/ \f  \
        //
        Region regionB = graph.getEdgeTarget(trueEdgeA);
        if (regionB.equals(regionA)) {
            return null;
        }
        BasicBlock exitBlockB = Regions.getDeepExitBasicBlock(graph, regionB);
        if (exitBlockB == null) {
            return null;
        }
        BasicBlockAnalysisData dataB = (BasicBlockAnalysisData) exitBlockB.getData();
        if (dataB.getStatementCount() != 1) {
            return null;
        }
        if (Regions.getSuccessorCountOf(graph, regionB, false) != 2) {
            return null;
        }
        Iterator<Edge> itE = Regions.getOutgoingEdgesOf(graph, regionB, false).iterator();
        Edge e1 = itE.next();
        Edge e2 = itE.next();
        Edge trueEdgeB = null;
        Edge falseEdgeB = null;
        if (Boolean.TRUE.equals(e1.getValue())) {
            trueEdgeB = e1;
        } else if (Boolean.TRUE.equals(e2.getValue())) {
            trueEdgeB = e2;
        }
        if (Boolean.FALSE.equals(e1.getValue())) {
            falseEdgeB = e1;
        } else if (Boolean.FALSE.equals(e2.getValue())) {
            falseEdgeB = e2;
        }
        if (trueEdgeB == null || falseEdgeB == null) {
            return null;
        }
        if (graph.getEdgeTarget(falseEdgeB).equals(graph.getEdgeTarget(falseEdgeA))) {
            return new LogicalRegion(LogicalType.AND, regionA, regionB, 
                                     trueEdgeA, falseEdgeA, trueEdgeB, falseEdgeB);
        } else if (graph.getEdgeTarget(trueEdgeB).equals(graph.getEdgeTarget(falseEdgeA))) {
            return new LogicalRegion(LogicalType.AND_INVERT_B, regionA, regionB, 
                                     trueEdgeA, falseEdgeA, trueEdgeB, falseEdgeB);            
        }
        return null;
    }

    private Region recognizeOr(DirectedGraph<Region, Edge> graph, Region regionA,
                               Edge trueEdgeA, Edge falseEdgeA) {
        //
        // check for OR
        //
        //     A
        //   f/ \t
        //   B   \
        // f/ \t  \
        //
        Region regionB = graph.getEdgeTarget(falseEdgeA);
        if (regionB.equals(regionA)) {
            return null;
        }
        BasicBlock exitBlockB = Regions.getDeepExitBasicBlock(graph, regionB);
        if (exitBlockB == null) {
            return null;
        }
        BasicBlockAnalysisData dataB = (BasicBlockAnalysisData) exitBlockB.getData();
        if (dataB.getStatementCount() != 1) {
            return null;
        }
        if (Regions.getSuccessorCountOf(graph, regionB, false) != 2) {
            return null;
        }
        Iterator<Edge> itE = Regions.getOutgoingEdgesOf(graph, regionB, false).iterator();
        Edge e1 = itE.next();
        Edge e2 = itE.next();
        Edge trueEdgeB = null;
        Edge falseEdgeB = null;
        if (Boolean.TRUE.equals(e1.getValue())) {
            trueEdgeB = e1;
        } else if (Boolean.TRUE.equals(e2.getValue())) {
            trueEdgeB = e2;
        }
        if (Boolean.FALSE.equals(e1.getValue())) {
            falseEdgeB = e1;
        } else if (Boolean.FALSE.equals(e2.getValue())) {
            falseEdgeB = e2;
        }
        if (trueEdgeB == null || falseEdgeB == null) {
            return null;
        }
        if (graph.getEdgeTarget(trueEdgeB).equals(graph.getEdgeTarget(trueEdgeA))) {
            return new LogicalRegion(LogicalType.OR, regionA, regionB, 
                                     trueEdgeA, falseEdgeA, trueEdgeB, falseEdgeB);
        } else if (graph.getEdgeTarget(falseEdgeB).equals(graph.getEdgeTarget(trueEdgeA))) {
            return new LogicalRegion(LogicalType.OR_INVERT_B, regionA, regionB, 
                                     trueEdgeA, falseEdgeA, trueEdgeB, falseEdgeB);            
        }
        return null;
    }
    
    public Region recognize(DirectedGraph<Region, Edge> graph, Region regionA) {
        if (Regions.getSuccessorCountOf(graph, regionA, false) != 2) {
            return null;
        }
        Iterator<Edge> itE = Regions.getOutgoingEdgesOf(graph, regionA, false).iterator();
        Edge e1 = itE.next();
        Edge e2 = itE.next();
        Edge trueEdgeA = null;
        Edge falseEdgeA = null;
        if (Boolean.TRUE.equals(e1.getValue())) {
            trueEdgeA = e1;
        } else if (Boolean.TRUE.equals(e2.getValue())) {
            trueEdgeA = e2;
        }
        if (Boolean.FALSE.equals(e1.getValue())) {
            falseEdgeA = e1;
        } else if (Boolean.FALSE.equals(e2.getValue())) {
            falseEdgeA = e2;
        }
        if (trueEdgeA == null || falseEdgeA == null) {
            return null;
        }
        
        Region structuredRegion = recognizeAnd(graph, regionA, trueEdgeA, falseEdgeA);
        if (structuredRegion == null) {
            structuredRegion = recognizeOr(graph, regionA, trueEdgeA, falseEdgeA);
        }
        
        return structuredRegion;
    }    
}
