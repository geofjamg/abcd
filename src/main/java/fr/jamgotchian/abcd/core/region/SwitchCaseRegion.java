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
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SwitchCaseRegion extends AbstractRegion {

    private final Region switchRegion;

    private final List<CaseRegion> caseRegions;

    SwitchCaseRegion(Region switchRegion, List<CaseRegion> caseRegions) {
        if (switchRegion == null) {
            throw new IllegalArgumentException("switchRegion == null");
        }
        if (caseRegions == null) {
            throw new IllegalArgumentException("caseRegions == null");
        }
        if (caseRegions.isEmpty()) {
            throw new IllegalArgumentException("caseRegions.isEmpty()");
        }
        this.switchRegion = switchRegion;
        this.caseRegions = caseRegions;
        switchRegion.setParent(this);
        for (CaseRegion _case : caseRegions) {
            if (_case.getRegion() != null) {
                _case.getRegion().setParent(this);
            }
        }
    }

    public RegionType getType() {
        return RegionType.SWITCH_CASE;
    }

    public Region getEntryRegion() {
         return switchRegion;
    }

    public Region getExitRegion() {
        return null;
    }

    public Region getSwitchRegion() {
        return switchRegion;
    }

    public List<CaseRegion> getCaseRegions() {
        return caseRegions;
    }

    public Collection<Region> getChildRegions() {
        Set<Region> regions = new HashSet<Region>();
        regions.add(switchRegion);
        for (CaseRegion _case : caseRegions) {
            if (_case.getRegion() != null) {
                regions.add(_case.getRegion());
            }
        }
        return regions;
    }

    public Collection<Edge> getChildEdges() {
        Set<Edge> edges = new HashSet<Edge>();
        for (CaseRegion _case : caseRegions) {
            edges.add(_case.getIncomingEdge());
            if (_case.getOutgoingEdge() != null) {
                edges.add(_case.getOutgoingEdge());
            }
        }
        return edges;
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveHandlers(graph, switchRegion, this);
        Region switchExitRegion = null;
        for (CaseRegion _case : caseRegions) {
            graph.removeEdge(_case.getIncomingEdge());
            if (_case.getOutgoingEdge() != null
                    && _case.getRegion() != null) {
                switchExitRegion = graph.getEdgeTarget(_case.getOutgoingEdge());
                graph.removeEdge(_case.getOutgoingEdge());
                graph.removeVertex(_case.getRegion());
            }
        }
        graph.addEdge(this, switchExitRegion, new EdgeImpl());
        Regions.moveIncomingEdges(graph, switchRegion, this);
        graph.removeVertex(switchRegion);
    }
}
