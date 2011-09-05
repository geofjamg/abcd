/*
 * Copyright (C) 2011 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *  *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package fr.jamgotchian.abcd.core.region;

import fr.jamgotchian.abcd.core.controlflow.Edge;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class InlinedFinallyBreakRecognizer implements RegionRecognizer {

    public Region recognize(RegionGraph graph, Region regionA) {
        if (graph.getSuccessorCountOf(regionA, false) != 1) {
            return null;
        }
        Edge edgeAB = graph.getFirstOutgoingEdgeOf(regionA, false);
        Region regionB = graph.getEdgeTarget(edgeAB);
        if (graph.getSuccessorCountOf(regionB, false) != 0) {
            return null;
        }
        if (graph.getPredecessorCountOf(regionB, false) != 1) {
            return null;
        }
        if (!regionB.isBreak()) {
            return null;
        }
        for (Region regionC : graph.getSuccessorsOf(regionA, true)) {
            if (Regions.sameInstructions(regionB, regionC)) {
                return new InlinedFinallyBreakRegion(regionA, edgeAB, regionB);
            }
        }
        return null;
    }

}
