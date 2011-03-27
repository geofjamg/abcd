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

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.Collection;
import java.util.Collections;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BasicBlockRegion implements Region {

    private final BasicBlock block;

    BasicBlockRegion(BasicBlock block) {
        if (block == null) {
            throw new ABCDException("block == null");
        }
        this.block = block;
    }

    public RegionType getType() {
        return RegionType.BASIC_BLOCK;
    }

    public String getTypeName() {
        return getType().toString();
    }

    public BasicBlock getBlock() {
        return block;
    }

    public Region getEntryRegion() {
        return null;
    }

    public Region getExitRegionIfUnique() {
        return null;
    }

    public BasicBlock getEntryBasicBlock() {
        return block;
    }

    public BasicBlock getExitBasicBlockIfUnique() {
        return block;
    }

    public Collection<Region> getChildRegions() {
        return Collections.emptyList();
    }

    public Collection<BasicBlock> getChildBasicBlocks() {
        return Collections.singleton(block);
    }

    public Collection<Edge> getChildEdges() {
        return Collections.emptySet();
    }

    @Override
    public RegionName getName() {
        return new RegionName(block);
    }

    public Collection<Region> getBreakRegions() {
        return Collections.emptySet();
    }

    public void addBreakRegion(Collection<Region> regions) {
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        throw new ABCDException("Cannot reduce a basic block region");
    }

    @Override
    public String toString() {
        return getName().toString();
    }
}