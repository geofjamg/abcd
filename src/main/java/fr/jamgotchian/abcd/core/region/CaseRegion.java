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

import fr.jamgotchian.abcd.core.controlflow.CaseValues;
import fr.jamgotchian.abcd.core.controlflow.Edge;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CaseRegion {

    private final Region region;
    
    private final Edge incomingEdge;
    
    private final Edge outgoingEdge;
    
    private final CaseValues values;

    CaseRegion(Region region, Edge incomingEdge, Edge outgoingEdge, CaseValues values) {
        this.region = region;
        this.incomingEdge = incomingEdge;
        this.outgoingEdge = outgoingEdge;
        this.values = values;
    }

    public Region getRegion() {
        return region;
    }

    public Edge getIncomingEdge() {
        return incomingEdge;
    }

    public Edge getOutgoingEdge() {
        return outgoingEdge;
    }

    public CaseValues getValues() {
        return values;
    }
}
