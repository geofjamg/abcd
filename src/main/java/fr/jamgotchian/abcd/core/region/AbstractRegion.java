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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
abstract class AbstractRegion implements Region {

    private BreakTargetStatus breakTargetStatus;

    private int breakLoopID;

    public AbstractRegion() {
        breakTargetStatus = BreakTargetStatus.NONE;
        breakLoopID = -1;
    }

    public String getTypeName() {
        return getType().toString();
    }

    public <T> Collection<T> getChildRegions(Class<T> clazz) {
        List<T> regions = new ArrayList<T>();
        addChildRegions(regions, clazz);
        return regions;
    }

    public <T> void addChildRegions(Collection<T> regions, Class<T> clazz) {
        if (getClass() == clazz) {
            regions.add((T) this);
        }
        for (Region child : getChildRegions()) {
            child.addChildRegions(regions, clazz);
        }
    }

    public BreakTargetStatus getBreakTargetStatus() {
        return breakTargetStatus;
    }

    public int getBreakLoopID() {
        return breakLoopID;
    }

    public void setBreakLoopID(int breakLoopID) {
        this.breakLoopID = breakLoopID;
    }

    public void setBreakTargetStatus(BreakTargetStatus breakTargetStatus) {
//        if (breakTargetStatus.ordinal() < this.breakTargetStatus.ordinal()) {
//            throw new ABCDException("Cannot change break target status from "
//                    + this.breakTargetStatus + " to " + breakTargetStatus);
//        }
        this.breakTargetStatus = breakTargetStatus;
    }

    public RegionName getName() {
        return getEntryRegion().getName().getParent();
    }

    @Override
    public String toString() {
        return getName().toString();
    }
}
