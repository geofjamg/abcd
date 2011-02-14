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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SwitchCaseRegion extends AbstractRegion {

    private final Set<Edge> internalEdges;

    private final Region switchRegion;
    
    private final List<Region> caseRegions;

    SwitchCaseRegion(Set<Edge> internalEdges,
                     Region switchRegion, List<Region> caseRegions) {
        if (internalEdges == null) {
            throw new ABCDException("internalEdges == null");
        }                                
        if (switchRegion == null) {
            throw new ABCDException("switchRegion == null");
        }
        if (caseRegions == null) {
            throw new ABCDException("caseRegions == null");
        }
        if (caseRegions.isEmpty()) {
            throw new ABCDException("caseRegions.isEmpty()");
        }
        this.internalEdges = Collections.unmodifiableSet(internalEdges);
        this.switchRegion = switchRegion;
        this.caseRegions = Collections.unmodifiableList(caseRegions);
    }
    
    public RegionType getType() {
        return RegionType.SWITCH_CASE;
    }

    public BasicBlock getEntryBlock() {
        return switchRegion.getEntryBlock();
    }

    public BasicBlock getExitBlock() {
        return null;
    }
   
    public Region getEntryRegion() {
         return switchRegion;
    }

    public Region getSwitchRegion() {
        return switchRegion;
    }

    public List<Region> getCaseRegions() {
        return caseRegions;
    }
    
    public Collection<Region> getInternalRegions() {
        List<Region> subRegions = new ArrayList<Region>(caseRegions.size() + 1);
        subRegions.add(switchRegion);
        subRegions.addAll(caseRegions);
        return subRegions;
    }

    public Set<Edge> getInternalEdges() {
        return internalEdges;
    }
}
