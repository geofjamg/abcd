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
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LabeledRegion extends AbstractRegion {

    private final Region bodyRegion;

    public LabeledRegion(Region bodyRegion) {
        this.bodyRegion = bodyRegion;
    }

    public RegionType getType() {
        return RegionType.LABELED;
    }

    public Region getBodyRegion() {
        return bodyRegion;
    }

    public Region getEntryRegion() {
        return bodyRegion;
    }

    public Region getExitRegion() {
        return bodyRegion;
    }

    public List<Region> getChildRegions() {
        return Arrays.asList(bodyRegion);
    }

    public Collection<Edge> getChildEdges() {
        return Collections.emptySet();
    }

    public void reduce(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveIncomingEdges(graph, bodyRegion, this);
        Regions.moveOutgoingEdges(graph, bodyRegion, this);
        graph.removeVertex(bodyRegion);
    }

}
