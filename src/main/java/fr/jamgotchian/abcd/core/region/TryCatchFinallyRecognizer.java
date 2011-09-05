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
import fr.jamgotchian.abcd.core.controlflow.ExceptionHandlerInfo;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TryCatchFinallyRecognizer implements RegionRecognizer {

    public TryCatchFinallyRegion recognizeTryCatch(RegionGraph graph, Region regionA,
                                                   Collection<Region> handlersOfA, Region regionB) {
        List<CatchRegion> catchRegions = new ArrayList<CatchRegion>();

        for (Region regionC : handlersOfA) {
            Collection<Region> successorsOfC = graph.getSuccessorsOf(regionC, false);
            if (successorsOfC.size() != 1
                    || !successorsOfC.iterator().next().equals(regionB)) {
                continue;
            }
            if (graph.getPredecessorCountOf(regionC, true) != 1) {
                continue;
            }
            Edge edgeAC = graph.getEdge(regionA, regionC);
            Edge edgeCB = graph.getEdge(regionC, regionB);
//            if (!Regions.sameHandlers(graph, regionA, regionC)) {
//                return null;
//            }
            catchRegions.add(new CatchRegion(regionC, edgeAC, edgeCB));
        }

        if (catchRegions.isEmpty()) {
            return null;
        }

        Edge edgeAB = graph.getEdge(regionA, regionB);

        return new TryCatchFinallyRegion(regionA, edgeAB, null, null, catchRegions, null);
    }

    public TryCatchFinallyRegion recognizeTryCatchFinally(RegionGraph graph,
                                                   Region regionA, Collection<Region> handlersOfA,
                                                   Region regionB, Region regionD) {
        List<CatchRegion> catchRegions = new ArrayList<CatchRegion>();
        CatchRegion finallyRegion = null;

        for (Region regionC : handlersOfA) {
            Collection<Region> successorsOfC = graph.getSuccessorsOf(regionC, false);
            if (successorsOfC.size() != 1
                    || !successorsOfC.iterator().next().equals(regionD)) {
                continue;
            }
            if (graph.getPredecessorCountOf(regionC, true) != 1) {
                continue;
            }
            Edge edgeAC = graph.getEdge(regionA, regionC);
            Edge edgeCD = graph.getEdge(regionC, regionD);

            CatchRegion catchRegion = new CatchRegion(regionC, edgeAC, edgeCD);

            ExceptionHandlerInfo handlerInfo = (ExceptionHandlerInfo) edgeAC.getValue();
            if (handlerInfo.getClassName() == null) {
                // should not have more than one finally clause
                if (finallyRegion != null) {
                    return null;
                }
//                if (!Regions.sameHandlers(graph, regionB, regionC)) {
//                    return null;
//                }
                if (!Regions.sameInstructions(regionB, regionC)) {
                    return null;
                }
                finallyRegion = catchRegion;
            } else {
//                if (!Regions.sameHandlers(graph, regionA, regionC)) {
//                    return null;
//                }
                catchRegions.add(catchRegion);
            }
        }

        if (catchRegions.isEmpty() && finallyRegion == null) {
            return null;
        }

        Edge edgeAB = graph.getEdge(regionA, regionB);
        Edge edgeBD = graph.getEdge(regionB, regionD);

        return new TryCatchFinallyRegion(regionA, edgeAB, regionB, edgeBD, catchRegions,
                                  finallyRegion);
    }

    public Region recognize(RegionGraph graph, Region regionA) {
        Collection<Region> successorsOfA = graph.getSuccessorsOf(regionA, false);
        if (successorsOfA.size() != 1) {
            return null;
        }
        Collection<Region> handlersOfA = graph.getSuccessorsOf(regionA, true);
        if (handlersOfA.isEmpty()) {
            return null;
        }
        Region regionB = successorsOfA.iterator().next();

        //
        // check for try catch region
        //
        //     A
        //     |  \  \
        //     |  C1 C2  handler region Ci
        //     |  /  /
        //     B
        //
        TryCatchFinallyRegion structuredRegion = recognizeTryCatch(graph, regionA, handlersOfA, regionB);
        if (structuredRegion != null) {
            return structuredRegion;
        }

        //
        // check for try (catch) finally region
        //
        //     A
        //     |  \  \
        //     B  C1 C2  handler region Ci
        //     |  /  /
        //     D
        //
        if (graph.getSuccessorCountOf(regionB, false) != 1
                || graph.getPredecessorCountOf(regionB, false) != 1) {
            return null;
        }
        Region regionD = graph.getFirstSuccessorOf(regionB, false);

        return recognizeTryCatchFinally(graph, regionA, handlersOfA, regionB, regionD);
    }

}
