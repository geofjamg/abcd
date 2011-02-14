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
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BlockRegion extends AbstractRegion {
 
    private final Set<Edge> internalEdges;
    
    private final List<Region> regions;
    
    BlockRegion(Set<Edge> internalEdges, List<Region> regions) {
        if (internalEdges == null) {
            throw new ABCDException("internalEdges == null");
        }                           
        if (regions == null) {
            throw new ABCDException("regions == null");
        }
        if (regions.size() < 2) {
            throw new ABCDException("regions < 2");
        }
        this.internalEdges = Collections.unmodifiableSet(internalEdges);
        this.regions = Collections.unmodifiableList(regions);
    }
    
    public RegionType getType() {
        return RegionType.BLOCK;
    }

    public BasicBlock getEntryBlock() {
        return regions.get(0).getEntryBlock();
    }

    public BasicBlock getExitBlock() {
        return regions.get(regions.size() - 1).getExitBlock();
    }

    public Region getEntryRegion() {
        return regions.get(0);
    }

    public List<Region> getRegions() {
        return regions;
    }

    public Collection<Region> getInternalRegions() {
        return regions;
    }

    public Set<Edge> getInternalEdges() {
        return internalEdges;
    }
}
