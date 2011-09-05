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
import fr.jamgotchian.abcd.core.controlflow.CaseValues;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.util.Sets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SwitchCaseRecognizer implements RegionRecognizer {

    public Region recognize(RegionGraph graph, Region regionA) {
        //
        // check switch case region
        //
        //      A
        //   / |  |  \
        //  BO B1 | default
        //   \ |  |  /
        //      C
        //
        BasicBlock blockA = Regions.getDeepExitBasicBlock(graph, regionA);
        if (blockA == null) {
            return null;
        }
        if (blockA.getType() == null || blockA.getType() != BasicBlockType.SWITCH) {
            return null;
        }
        Set<Edge> edgesAB
                = new HashSet<Edge>(graph.getOutgoingEdgesOf(regionA, false));
        if (edgesAB.isEmpty()) {
            return null;
        }

        List<CaseRegion> caseRegions = new ArrayList<CaseRegion>();

        for (Iterator<Edge> itE = edgesAB.iterator(); itE.hasNext();) {
            Edge edgeAB = itE.next();
            Region regionB = graph.getEdgeTarget(edgeAB);
            BasicBlock exitBlockOfRegionB = Regions.getDeepExitBasicBlock(graph, regionB);
            if (exitBlockOfRegionB != null
                    && exitBlockOfRegionB.getType() == BasicBlockType.RETURN) {
                Edge edgeBExit = graph.getFirstOutgoingEdgeOf(regionB, false);
                CaseValues value = (CaseValues) edgeAB.getValue();
                caseRegions.add(new CaseRegion(regionB, edgeAB, edgeBExit, value));
                itE.remove();
            }
        }

        if (edgesAB.size() > 0) {
            // find join region
            List<Set<Region>> regionsBC = new ArrayList<Set<Region>>();
            for (Edge edgeAB : edgesAB) {
                Set<Region> regionBC = new HashSet<Region>();
                Region regionB = graph.getEdgeTarget(edgeAB);
                regionBC.add(regionB);
                regionBC.add(graph.getFirstSuccessorOf(regionB, false));
                regionsBC.add(regionBC);
            }

            Set<Region> intersect = Sets.intersection(regionsBC);
            if (intersect.size() == 1) {
                Region regionC = intersect.iterator().next();

                for (Edge edgeAB : edgesAB) {
                    Region regionB = graph.getEdgeTarget(edgeAB);
                    CaseValues value = (CaseValues) edgeAB.getValue();
                    if (regionB.equals(regionC)) { // empty case
                        caseRegions.add(new CaseRegion(null, edgeAB, null, value));
                    } else {
                        Edge edgeBC = graph.getFirstOutgoingEdgeOf(regionB, false);
                        caseRegions.add(new CaseRegion(regionB, edgeAB, edgeBC, value));
                    }
                }

                edgesAB.clear();
            }
        }

        if (edgesAB.size() > 0) {
            return null;
        }

        // to order cases by value (with default value at the end)
        Collections.sort(caseRegions, new Comparator<CaseRegion>() {
            public int compare(CaseRegion o1, CaseRegion o2) {
                return o1.getValues().compareTo(o2.getValues());
            }
        });

        return new SwitchCaseRegion(regionA, caseRegions);
    }

}
