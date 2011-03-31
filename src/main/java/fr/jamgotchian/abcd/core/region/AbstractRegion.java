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

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
abstract class AbstractRegion implements Region {

    private boolean breakTarget;

    public AbstractRegion() {
    }

    public String getTypeName() {
        return getType().toString();
    }

    public Collection<Region> getBreakRegions() {
        Set<Region> regions = new HashSet<Region>();
        addBreakTargetRegion(regions);
        return regions;
    }

    public void addBreakTargetRegion(Collection<Region> regions) {
        for (Region child : getChildRegions()) {
            child.addBreakTargetRegion(regions);
        }
    }

    public boolean isBreakTarget() {
        return breakTarget;
    }

    public void setBreakTarget(boolean breakTarget) {
        this.breakTarget = breakTarget;
    }

    public RegionName getName() {
        return getEntryRegion().getName().getParent();
    }

    @Override
    public String toString() {
        return getName().toString();
    }
}
