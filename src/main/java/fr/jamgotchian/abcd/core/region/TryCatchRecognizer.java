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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TryCatchRecognizer implements RegionRecognizer {

    public TryCatchRegion recognizeForm1(DirectedGraph<Region, Edge> graph, Region regionA,
                                         Collection<Region> handlersOfA, Region regionB) {
        List<CatchRegion> catchs = new ArrayList<CatchRegion>();

        for (Region regionC : handlersOfA) {
            Collection<Region> successorsOfC = Regions.getSuccessorsOf(graph, regionC, false);
            if (successorsOfC.size() != 1
                    || !successorsOfC.iterator().next().equals(regionB)) {
                continue;
            }
            if (Regions.getPredecessorCountOf(graph, regionC, true) != 1) {
                continue;
            }
            Edge edgeAC = graph.getEdge(regionA, regionC);
            Edge edgeCB = graph.getEdge(regionC, regionB);
            String exceptionClassName = (String) edgeAC.getValue();
            if (exceptionClassName == null) {
                exceptionClassName = Throwable.class.getName();
            }
            catchs.add(new CatchRegion(regionC, edgeAC, edgeCB, exceptionClassName));
        }

        if (catchs.isEmpty()) {
            return null;
        }

        Edge edgeAB = graph.getEdge(regionA, regionB);

        return new TryCatchRegion(regionA, edgeAB, null, null, catchs);
    }

    public TryCatchRegion recognizeForm2(DirectedGraph<Region, Edge> graph, Region regionA,
                                         Collection<Region> handlersOfA, Region regionB, Region regionD) {
        List<CatchRegion> catchs = new ArrayList<CatchRegion>();

        for (Region regionC : handlersOfA) {
            Collection<Region> successorsOfC = Regions.getSuccessorsOf(graph, regionC, false);
            if (successorsOfC.size() != 1
                    || !successorsOfC.iterator().next().equals(regionD)) {
                continue;
            }
            if (Regions.getPredecessorCountOf(graph, regionC, true) != 1) {
                continue;
            }
            Edge edgeAC = graph.getEdge(regionA, regionC);
            Edge edgeCD = graph.getEdge(regionC, regionD);
            String exceptionClassName = (String) edgeAC.getValue();
            if (exceptionClassName == null) {
                exceptionClassName = Throwable.class.getName();
            }
            catchs.add(new CatchRegion(regionC, edgeAC, edgeCD, exceptionClassName));
        }

        if (catchs.isEmpty()) {
            return null;
        }

        Edge edgeAB = graph.getEdge(regionA, regionB);
        Edge edgeBD = graph.getEdge(regionB, regionD);

        return new TryCatchRegion(regionA, edgeAB, regionB, edgeBD, catchs);
    }

    public Region recognize(DirectedGraph<Region, Edge> graph, Region regionA) {
        Collection<Region> successorsOfA = Regions.getSuccessorsOf(graph, regionA, false);
        Collection<Region> handlersOfA = Regions.getSuccessorsOf(graph, regionA, true);
        if (successorsOfA.size() != 1) {
            return null;
        }
        if (handlersOfA.isEmpty()) {
            return null;
        }
        Region regionB = successorsOfA.iterator().next();

        //
        // check for try catch region form 1
        //
        //     A
        //     |  \  \
        //     |  C1 C2  handler region Ci
        //     |  /  /
        //     B
        //
        TryCatchRegion structuredRegion = recognizeForm1(graph, regionA, handlersOfA, regionB);
        if (structuredRegion != null) {
            return structuredRegion;
        }

        //
        // check for try catch region form 2
        //
        //     A
        //     |  \  \
        //     B  C1 C2  handler region Ci
        //     |  /  /
        //     D
        //
        if (Regions.getSuccessorCountOf(graph, regionB, false) != 1
                || Regions.getPredecessorCountOf(graph, regionB, false) != 1) {
            return null;
        }
        Region regionD = Regions.getFirstSuccessorOf(graph, regionB, false);

        structuredRegion = recognizeForm2(graph, regionA, handlersOfA, regionB, regionD);
        if (structuredRegion != null) {
            return structuredRegion;
        }

        return null;
    }

}
