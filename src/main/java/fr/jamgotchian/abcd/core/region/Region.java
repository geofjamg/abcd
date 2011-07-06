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
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.Collection;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface Region {

    RegionName getName();

    RegionType getType();

    String getTypeName();

    Region getParent();

    void setParent(Region parent);

    Region getAncestor(Set<RegionType> types);

    Region getEntryRegion();

    Region getExitRegion();

    Collection<Region> getChildRegions();

    Collection<Edge> getChildEdges();

    <T> Collection<T> getChildRegions(Class<T> clazz);

    <T> void addChildRegions(Collection<T> regions, Class<T> clazz);

    void setBreakTargetStatus(BreakTargetStatus breakTargetStatus);

    BreakTargetStatus getBreakTargetStatus();

    int getBreakLoopID();

    void setBreakLoopID(int breakLoopID);

    void collapse(MutableDirectedGraph<Region, Edge> graph);
}
