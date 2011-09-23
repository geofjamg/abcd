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

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
abstract class AbstractRegion implements Region {

    private Region parent;

    private boolean _break;

    private final Set<RegionAttribute> attributes;

    AbstractRegion() {
        _break = false;
        attributes = EnumSet.noneOf(RegionAttribute.class);
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

    public Region getParent() {
        return parent;
    }

    public void setParent(Region parent) {
        this.parent = parent;
    }

    public Region getAncestor(Set<RegionType> types) {
        if (parent == null) {
            return null;
        }
        if (types.contains(parent.getType())) {
            return parent;
        } else {
            return parent.getAncestor(types);
        }
    }

    public boolean isBreak() {
        return _break;
    }

    public void setBreak(boolean _break) {
        this._break = _break;
    }

    public boolean hasAttribute(RegionAttribute attr) {
        return attributes.contains(attr);
    }

    public void addAttribute(RegionAttribute attr) {
        attributes.add(attr);
    }

    public void removeAttribute(RegionAttribute attr) {
        attributes.remove(attr);
    }

    public RegionName getName() {
        return getEntryRegion().getName().getParent();
    }

    @Override
    public String toString() {
        return getName().toString();
    }
}
