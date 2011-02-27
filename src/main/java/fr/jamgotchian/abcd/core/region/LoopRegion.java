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
import fr.jamgotchian.abcd.core.controlflow.Edge;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LoopRegion extends AbstractRegion {

    private final LoopType loopType;
    
    private final Region loopTailRegion;

    private final Edge backEdge;

    private final List<LoopSubRegion> subRegions;
    
    public LoopRegion(LoopType loopType, Edge backEdge, Region loopTailRegion,
                      List<LoopSubRegion> subRegions) {
        if (loopType == null) {
            throw new ABCDException("loopType == null");
        }
        if (backEdge == null) {
            throw new ABCDException("backEdge == null");
        }                               
        if (loopTailRegion == null) {
            throw new ABCDException("loopTailRegion == null");
        }
        if (subRegions == null) {
            throw new ABCDException("subRegions == null");
        }
        this.loopType = loopType;
        this.backEdge = backEdge;
        this.loopTailRegion = loopTailRegion;
        this.subRegions = subRegions;
    }

    public RegionType getType() {
        return RegionType.LOOP;
    }
    
    public LoopType getLoopType() {
        return loopType;
    }

    public Region getEntryRegion() {
        if (subRegions.isEmpty()) {
            return loopTailRegion;
        } else {
            return subRegions.get(0).getLoopRegion();
        }
    }

    public Region getExitRegionIfUnique() {
        return loopTailRegion;
    }

    public Region getLoopTailRegion() {
        return loopTailRegion;
    }

    public Edge getLoopBackEdge() {
        return backEdge;
    }

    public List<LoopSubRegion> getSubRegions() {
        return Collections.unmodifiableList(subRegions);
    }
       
    public Collection<Region> getInternalRegions() {
        List<Region> regions = new ArrayList<Region>();
        regions.add(loopTailRegion);
        for (LoopSubRegion b : subRegions) {
            regions.add(b.getLoopRegion());
        }
        return regions;
    }

    public Collection<Edge> getInternalEdges() {
        Set<Edge> edges = new HashSet<Edge>();
        edges.add(backEdge);
        for (LoopSubRegion b : subRegions) {
            edges.add(b.getLoopEdge());
            edges.add(b.getLoopExitEdge());
        }
        return edges;
    }
}
