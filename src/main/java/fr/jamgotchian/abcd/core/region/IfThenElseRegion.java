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

import fr.jamgotchian.abcd.core.controlflow.Edge;
import java.util.Arrays;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class IfThenElseRegion extends AbstractRegion {

    protected final Edge thenEdge;

    protected final Edge elseEdge;

    protected final Region ifRegion;

    protected final Region thenRegion;

    protected final Region elseRegion;

    IfThenElseRegion(Edge thenEdge, Edge elseEdge, Region ifRegion,
                     Region thenRegion, Region elseRegion) {
        if (thenEdge == null) {
            throw new IllegalArgumentException("thenEdge == null");
        }
        if (elseEdge == null) {
            throw new IllegalArgumentException("elseEdge == null");
        }
        if (ifRegion == null) {
            throw new IllegalArgumentException("ifRegion == null");
        }
        if (thenRegion == null) {
            throw new IllegalArgumentException("thenRegion == null");
        }
        if (elseRegion == null) {
            throw new IllegalArgumentException("elseRegion == null");
        }
        this.thenEdge = thenEdge;
        this.elseEdge = elseEdge;
        this.ifRegion = ifRegion;
        this.thenRegion = thenRegion;
        this.elseRegion = elseRegion;
        ifRegion.setParent(this);
        thenRegion.setParent(this);
        elseRegion.setParent(this);
    }

    public Region getEntryRegion() {
         return ifRegion;
    }

    public Region getExitRegion() {
        return null;
    }

    public Region getIfRegion() {
        return ifRegion;
    }

    public Region getThenRegion() {
        return thenRegion;
    }

    public Region getElseRegion() {
        return elseRegion;
    }

    public List<Region> getChildRegions() {
        return Arrays.asList(ifRegion, thenRegion, elseRegion);
    }
}
