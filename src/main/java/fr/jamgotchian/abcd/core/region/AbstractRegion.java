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

    public BasicBlock getEntryBasicBlock() {
        return getEntryRegion().getEntryBasicBlock();
    }

    public BasicBlock getExitBasicBlockIfUnique() {
        Region region = getExitRegionIfUnique();
        if (region == null) {
            return null;
        } else {
            return region.getExitBasicBlockIfUnique();
        }
    }

    public Collection<BasicBlock> getChildBasicBlocks() {
        Set<BasicBlock> blocks = new HashSet<BasicBlock>();
        for (Region region : getChildRegions()) {
            blocks.addAll(region.getChildBasicBlocks());
        }
        return blocks;
    }

    public Collection<Region> getBreakRegions() {
        Set<Region> regions = new HashSet<Region>();
        addBreakRegion(regions);
        return regions;
    }

    public void addBreakRegion(Collection<Region> regions) {
        for (Region child : getChildRegions()) {
            child.addBreakRegion(regions);
        }
    }

    public RegionName getName() {
        return getEntryRegion().getName().getParent();
    }
    
    @Override
    public String toString() {
        return getName().toString();
    }
}
