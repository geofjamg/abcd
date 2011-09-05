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
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class InfiniteLoopRegion extends AbstractRegion {

    private final Region loopRegion;

    private final Edge backEdge;

    public InfiniteLoopRegion(Edge backEdge, Region loopRegion) {
        if (backEdge == null) {
            throw new IllegalArgumentException("backEdge == null");
        }
        if (loopRegion == null) {
            throw new IllegalArgumentException("loopRegion == null");
        }
        this.backEdge = backEdge;
        this.loopRegion = loopRegion;
        loopRegion.setParent(this);
    }

    public RegionType getType() {
        return RegionType.INFINITE_LOOP;
    }

    public Region getEntryRegion() {
        return loopRegion;
    }

    public Region getExitRegion() {
        return loopRegion;
    }

    public Region getLoopRegion() {
        return loopRegion;
    }

    public List<Region> getChildRegions() {
        return Collections.singletonList(loopRegion);
    }

    public void reduce(RegionGraph graph) {
        graph.addRegion(this);
        Regions.moveHandlers(graph, loopRegion, this);
        graph.removeEdge(backEdge);
        Regions.moveIncomingEdges(graph, loopRegion, this);
        graph.removeRegion(loopRegion);
    }
}
