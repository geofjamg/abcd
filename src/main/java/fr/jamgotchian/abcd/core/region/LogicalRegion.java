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
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import fr.jamgotchian.abcd.core.util.Collections3;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LogicalRegion extends AbstractRegion {

    private final LogicalType type;

    private final Region regionA;

    private final Region regionB;

    private final Edge trueEdgeA;

    private final Edge falseEdgeA;

    private final Edge trueEdgeB;

    private final Edge falseEdgeB;

    LogicalRegion(LogicalType type, Region regionA, Region regionB,
                  Edge trueEdgeA, Edge falseEdgeA, Edge trueEdgeB, Edge falseEdgeB) {
        this.type = type;
        this.regionA = regionA;
        this.regionB = regionB;
        this.trueEdgeA = trueEdgeA;
        this.falseEdgeA = falseEdgeA;
        this.trueEdgeB = trueEdgeB;
        this.falseEdgeB = falseEdgeB;
    }

    public RegionType getType() {
        return RegionType.LOGICAL;
    }

    @Override
    public String getTypeName() {
        return super.getTypeName() + " (" + type + ")";
    }

    public LogicalType getLogicalType() {
        return type;
    }

    public Region getEntryRegion() {
        return regionA;
    }

    public Region getExitRegionIfUnique() {
        return regionB;
    }

    public Collection<Region> getInternalRegions() {
        return Arrays.asList(regionA, regionB);
    }

    public Set<Edge> getInternalEdges() {
        return Sets.newHashSet(trueEdgeA, falseEdgeA, trueEdgeB, falseEdgeB);
    }

    public Region getRegionA() {
        return regionA;
    }

    public Region getRegionB() {
        return regionB;
    }

    @Override
    public Edge createSyntheticEdge(Collection<Edge> edges) {
        Edge syntheticEdge = null;
        switch (type) {
            case AND:
                if (Collections3.sameContent(edges, Arrays.asList(trueEdgeB))) {
                    syntheticEdge = new EdgeImpl(Boolean.TRUE, trueEdgeB.isLoopExit());
                } else if (Collections3.sameContent(edges, Arrays.asList(falseEdgeB, falseEdgeA))) {
                    syntheticEdge = new EdgeImpl(Boolean.FALSE, falseEdgeB.isLoopExit() && falseEdgeA.isLoopExit());
                }
                break;

            case OR:
                if (Collections3.sameContent(edges, Arrays.asList(falseEdgeB))) {
                    syntheticEdge = new EdgeImpl(Boolean.FALSE, falseEdgeB.isLoopExit());
                } else if (Collections3.sameContent(edges, Arrays.asList(trueEdgeB, trueEdgeA))) {
                    syntheticEdge = new EdgeImpl(Boolean.TRUE, trueEdgeB.isLoopExit() && trueEdgeA.isLoopExit());
                }
                break;

            case AND_INVERT_B:
                if (Collections3.sameContent(edges, Arrays.asList(falseEdgeB))) {
                    syntheticEdge = new EdgeImpl(Boolean.FALSE, falseEdgeB.isExceptional());
                } else if (Collections3.sameContent(edges, Arrays.asList(trueEdgeB, falseEdgeA))) {
                    syntheticEdge = new EdgeImpl(Boolean.TRUE, trueEdgeB.isLoopExit() && falseEdgeA.isLoopExit());
                }
                break;

            case OR_INVERT_B:
                if (Collections3.sameContent(edges, Arrays.asList(trueEdgeB))) {
                    syntheticEdge = new EdgeImpl(Boolean.FALSE, trueEdgeB.isLoopExit());
                } else if (Collections3.sameContent(edges, Arrays.asList(falseEdgeB, trueEdgeA))) {
                    syntheticEdge = new EdgeImpl(Boolean.TRUE, falseEdgeB.isLoopExit() && trueEdgeA.isLoopExit());
                }
                break;
        }
        
        if (syntheticEdge == null) {
            throw new ABCDException("Cannot create synthetic edge");
        }
        return syntheticEdge;
    }

}
