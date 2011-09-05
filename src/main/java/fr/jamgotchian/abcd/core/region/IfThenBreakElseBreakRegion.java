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

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfThenBreakElseBreakRegion extends IfThenElseRegion {

    IfThenBreakElseBreakRegion(Edge thenEdge, Edge elseEdge, Region ifRegion,
                               Region thenRegion, Region elseRegion) {
        super(thenEdge, elseEdge, ifRegion, thenRegion, elseRegion);
    }

    public RegionType getType() {
        return RegionType.IF_THEN_BREAK_ELSE_BREAK;
    }

    public void reduce(RegionGraph graph) {
        graph.addRegion(this);
        Regions.moveHandlers(graph, ifRegion, this);
        Regions.moveIncomingEdges(graph, ifRegion, this);
        graph.removeEdge(thenEdge);
        graph.removeEdge(elseEdge);
        graph.removeRegion(ifRegion);
        graph.removeRegion(thenRegion);
        graph.removeRegion(elseRegion);
        setBreak(true);
    }
}
