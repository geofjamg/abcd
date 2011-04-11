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
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LoopRegion extends AbstractRegion {

    private final LoopType loopType;

    private final Region loopRegion;

    private final Edge backEdge;

    private final int loopID;

    public LoopRegion(LoopType loopType, Edge backEdge, Region loopRegion, int loopID) {
        if (loopType == null) {
            throw new ABCDException("loopType == null");
        }
        if (backEdge == null) {
            throw new ABCDException("backEdge == null");
        }
        if (loopRegion == null) {
            throw new ABCDException("loopRegion == null");
        }
        this.loopType = loopType;
        this.backEdge = backEdge;
        this.loopRegion = loopRegion;
        this.loopID = loopID;
    }

    public RegionType getType() {
        return RegionType.LOOP;
    }

    public LoopType getLoopType() {
        return loopType;
    }

    public Region getEntryRegion() {
        return loopRegion;
    }

    public Region getExitRegion() {
        return loopRegion;
    }

    public Region getLoopRegion() {
        return loopRegion;
    }

    public Edge getBackEdge() {
        return backEdge;
    }

    public int getLoopID() {
        return loopID;
    }

    public Collection<Region> getChildRegions() {
        return Sets.newHashSet(loopRegion);
    }

    public Collection<Edge> getChildEdges() {
        return Sets.newHashSet(backEdge);
    }

    public void collapse(MutableDirectedGraph<Region, Edge> graph) {
        graph.addVertex(this);
        Regions.moveHandlers(graph, loopRegion, this);
        Regions.moveIncomingEdges(graph, loopRegion, this);
        graph.removeEdge(backEdge);
        IfThenBreakRegion ifThenBreak = null;
        switch (loopType) {
            case WHILE: {
                ifThenBreak = (IfThenBreakRegion) Regions.getDeepEntryRegion(graph, loopRegion);
                break;
            }

            case DO_WHILE: {
                ifThenBreak = (IfThenBreakRegion) Regions.getDeepExitRegion(graph, loopRegion);
                break;
            }

            case INFINITE: {
                List<IfThenBreakRegion> ifThenBreaks = new ArrayList<IfThenBreakRegion>();
                for (IfThenBreakRegion ifThenBreak2 : getChildRegions(IfThenBreakRegion.class)) {
                    if (ifThenBreak2.getBreakTargetRegion().getBreakTargetStatus() == BreakTargetStatus.UNASSIGNED) {
                        ifThenBreaks.add(ifThenBreak2);
                    }
                }
                if (ifThenBreaks.size() != 1) {
                    throw new ABCDException("ifThenBreaks.size() != 1");
                }
                ifThenBreak = ifThenBreaks.iterator().next();
                break;
            }

            default:
                throw new AssertionError();
        }

        Region breakTargetRegion = ifThenBreak.getBreakTargetRegion();
        if (breakTargetRegion.getBreakTargetStatus() == BreakTargetStatus.ASSIGNED) {
            throw new ABCDException("Break target already assigned to another loop");
        }
        breakTargetRegion.setBreakTargetStatus(BreakTargetStatus.ASSIGNED);
        breakTargetRegion.setBreakLoopID(loopID);

        boolean before = ifThenBreak.getBeforeThenRegion() != null
                && ifThenBreak.getBeforeThenEdge() != null;
        boolean after = ifThenBreak.getAfterThenRegion() != null;
        if (after && before) {
            graph.addVertex(ifThenBreak.getBeforeThenRegion());
            graph.addVertex(ifThenBreak.getAfterThenRegion());
            graph.addEdge(this, ifThenBreak.getBeforeThenRegion(), ifThenBreak.getBeforeThenEdge());
            graph.addEdge(ifThenBreak.getBeforeThenRegion(), ifThenBreak.getAfterThenRegion(), ifThenBreak.getThenBreakEdge());
            graph.addEdge(ifThenBreak.getAfterThenRegion(), breakTargetRegion, ifThenBreak.getAfterThenEdge());
            ifThenBreak.setBeforeThenRegion(null);
            ifThenBreak.setBeforeThenEdge(null);
            ifThenBreak.setAfterThenRegion(null);
            ifThenBreak.setAfterThenEdge(null);
        } else if (after && !before) {
            graph.addVertex(ifThenBreak.getAfterThenRegion());
            graph.addEdge(this, ifThenBreak.getAfterThenRegion(), ifThenBreak.getThenBreakEdge());
            graph.addEdge(ifThenBreak.getAfterThenRegion(), breakTargetRegion, ifThenBreak.getAfterThenEdge());
            ifThenBreak.setAfterThenRegion(null);
            ifThenBreak.setAfterThenEdge(null);
        } else if (!after && !before) {
            graph.addEdge(this, breakTargetRegion, ifThenBreak.getThenBreakEdge());
        }

        graph.removeVertex(loopRegion);
    }
}
