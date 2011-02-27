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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SwitchCaseRecognizer implements RegionRecognizer {

    public Region recognize(DirectedGraph<Region, Edge> graph, Region regionA) {
        //
        // check switch case region
        //
        //    ...    incomingExternalEdge
        //     A     region A
        //   / | \
        //  BO B1 B2 ... region Bi
        //   \ | /
        //     C     region D
        //    ...    outgoingExternalEdges
        //
        BasicBlock blockA = regionA.getExitBlockIfUnique();
        if (blockA.getType() == null || blockA.getType() != BasicBlockType.SWITCH) {
            return null;
        }
        Collection<Region> regionBi = graph.getSuccessorsOf(regionA);
        if (regionBi.isEmpty()) {
            return null;
        }
        Set<Region> regionCi = new HashSet<Region>();
        for (Region regionB : regionBi) {
            if (graph.getSuccessorCountOf(regionB) != 1 
                    || graph.getPredecessorCountOf(regionB) != 1) {
                return null;
            }
            regionCi.add(graph.getFirstSuccessorsOf(regionB));
        }
        if (regionCi.size() != 1) {
            return null;
        } 
        Map<Object, CaseRegion> caseRegions = new HashMap<Object, CaseRegion>();
        for (Region regionB : regionBi) {
            Edge incomingEdge = graph.getFirstIncomingEdgesOf(regionB);
            Edge outgoingEdge = graph.getFirstOutgoingEdgesOf(regionB);
            Object value = incomingEdge.getValue();
            caseRegions.put(value, new CaseRegion(regionB, incomingEdge, outgoingEdge, value));
        }
        return new SwitchCaseRegion(regionA, new ArrayList<CaseRegion>(caseRegions.values()));
    }

}
