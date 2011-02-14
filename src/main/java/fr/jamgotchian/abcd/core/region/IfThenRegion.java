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

import com.google.common.collect.Sets;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfThenRegion extends AbstractRegion {

    private final Edge beforeThenEdge;
    
    private final Edge afterThenEdge;
    
    private final Edge jumpEdge;

    private final Region ifRegion;
    
    private final Region thenRegion;

    private final boolean invertCondition;
    
    IfThenRegion(Edge beforeThenEdge, Edge afterThenEdge, Edge jumpEdge,
                        Region ifRegion, Region thenRegion, boolean invertCondition) {
        if (beforeThenEdge == null) {
            throw new ABCDException("beforeThenEdge == null");
        }
        if (afterThenEdge == null) {
            throw new ABCDException("afterThenEdge == null");
        }
        if (jumpEdge == null) {
            throw new ABCDException("jumpEdge == null");
        }                            
        if (ifRegion == null) {
            throw new ABCDException("ifRegion == null");
        }
        if (thenRegion == null) {
            throw new ABCDException("thenRegion == null");
        }
        this.beforeThenEdge = beforeThenEdge;
        this.afterThenEdge = afterThenEdge;
        this.jumpEdge = jumpEdge;
        this.ifRegion = ifRegion;
        this.thenRegion = thenRegion;
        this.invertCondition = invertCondition;
    }
    
    public RegionType getType() {
        return RegionType.IF_THEN;
    }

    public BasicBlock getEntryBlock() {
        return ifRegion.getEntryBlock();
    }

    public BasicBlock getExitBlock() {
        return null;
    }

    public Region getEntryRegion() {
         return ifRegion;
    }

    public Region getIfRegion() {
        return ifRegion;
    }

    public Region getThenRegion() {
        return thenRegion;
    }

    public boolean isInvertCondition() {
        return invertCondition;
    }
    
    public Collection<Region> getInternalRegions() {
        return Arrays.asList(ifRegion, thenRegion);
    }

    public Set<Edge> getInternalEdges() {
        return Sets.newHashSet(beforeThenEdge, afterThenEdge, jumpEdge);
    }
}
