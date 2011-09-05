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
import java.util.Arrays;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class WhileLoopRegion extends AbstractRegion {

    private final Edge backEdge;

    private final Region ifRegion;

    private final Edge thenEdge;

    private final Region thenRegion;

    private final Edge loopEdge;

    private final Region loopRegion;

    public WhileLoopRegion(Edge backEdge, Region ifRegion, Edge thenEdge,
                           Region thenRegion, Edge loopEdge, Region loopRegion) {
        if (backEdge == null) {
            throw new IllegalArgumentException("backEdge == null");
        }
        if (ifRegion == null) {
            throw new IllegalArgumentException("ifRegion == null");
        }
        if (thenEdge == null) {
            throw new IllegalArgumentException("thenEdge == null");
        }
        if (thenRegion == null) {
            throw new IllegalArgumentException("thenRegion == null");
        }
        if (loopEdge == null) {
            throw new IllegalArgumentException("loopEdge == null");
        }
        if (loopRegion == null) {
            throw new IllegalArgumentException("loopRegion == null");
        }
        this.backEdge = backEdge;
        this.ifRegion = ifRegion;
        this.thenEdge = thenEdge;
        this.thenRegion = thenRegion;
        this.loopEdge = loopEdge;
        this.loopRegion = loopRegion;
        loopRegion.setParent(this);
    }

    public RegionType getType() {
        return RegionType.WHILE_LOOP;
    }

    public Region getEntryRegion() {
        return ifRegion;
    }

    public Region getExitRegion() {
        return thenRegion;
    }

    public Region getLoopRegion() {
        return loopRegion;
    }

    public Region getIfRegion() {
        return ifRegion;
    }

    public List<Region> getChildRegions() {
        return Arrays.asList(ifRegion, loopRegion);
    }

    public void reduce(RegionGraph graph) {
        graph.addRegion(this);
        Regions.moveHandlers(graph, ifRegion, this);
        graph.removeEdge(backEdge);
        graph.removeEdge(thenEdge);
        graph.removeEdge(loopEdge);
        Regions.moveIncomingEdges(graph, ifRegion, this);
        graph.removeRegion(ifRegion);
        graph.removeRegion(loopRegion);
        graph.addEdge(this, thenRegion, new EdgeImpl());
    }
}
