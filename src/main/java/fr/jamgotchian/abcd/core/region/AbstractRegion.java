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
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class AbstractRegion implements Region {

    public AbstractRegion() {
    }

    public String getTypeName() {
        return getType().toString();
    }

    public BasicBlock getEntryBlock() {
        return getEntryRegion().getEntryBlock();
    }

    public BasicBlock getExitBlockIfUnique() {
        Region region = getExitRegionIfUnique();
        if (region == null) {
            return null;
        } else {
            return region.getExitBlockIfUnique();
        }
    }

    public Collection<BasicBlock> getInternalBlocks() {
        Set<BasicBlock> blocks = new HashSet<BasicBlock>();
        for (Region region : getInternalRegions()) {
            blocks.addAll(region.getInternalBlocks());
        }
        return blocks;
    }

    public RegionName getName() {
        return getEntryRegion().getName().getParent();
    }
    
    public Edge createSyntheticEdge(Collection<Edge> edges) {
        return new EdgeImpl();
    }
    
    @Override
    public String toString() {
        return getName().toString();
    }
}
