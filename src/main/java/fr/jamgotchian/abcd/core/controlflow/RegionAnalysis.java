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
package fr.jamgotchian.abcd.core.controlflow;

import fr.jamgotchian.abcd.core.OutputHandler;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RegionAnalysis {

    private static final Logger logger = Logger.getLogger(RegionAnalysis.class.getName());

    private final ControlFlowGraph cfg;

    public RegionAnalysis(ControlFlowGraph cfg) {
        this.cfg = cfg;
    }

    private boolean checkSequenceRegion(Region region) {
        if (region.getChildCount() == 2) {
            Iterator<Region> it = region.getChildren().iterator();
            Region child1 = it.next();
            Region child2 = it.next();
            if (child2.getEntry().equals(child1.getExit())) {
                region.setParentType(ParentType.SEQUENCE);
                child1.setChildType(ChildType.FIRST);
                child2.setChildType(ChildType.SECOND);
                return true;
            } else if (child1.getEntry().equals(child2.getExit())) {
                region.setParentType(ParentType.SEQUENCE);
                child2.setChildType(ChildType.FIRST);
                child1.setChildType(ChildType.SECOND);
                return true;
            }
        }
        return false;
    }

    private boolean checkIfThenRegion(Region region) {
        if (region.getChildCount() == 2) {
            Region ifRegion = null;
            Region thenRegion = null;
            for (Region child : region.getChildren()) {
                if (child.getEntry().equals(region.getEntry())) {
                    ifRegion = child;
                } else {
                    thenRegion = child;
                }
            }
            if (ifRegion != null && thenRegion != null) {
                BasicBlock ifBB = ifRegion.getEntry();
                BasicBlock thenBB = thenRegion.getEntry();
                Edge thenEdge = cfg.getEdge(ifBB, thenBB);
                Edge elseEdge = cfg.getEdge(ifBB, region.getExit());
                if (thenEdge != null && elseEdge != null) {
                    if (Boolean.TRUE.equals(thenEdge.getValue())
                            && Boolean.FALSE.equals(elseEdge.getValue())) {
                        region.setParentType(ParentType.IF_THEN);
                        ifRegion.setChildType(ChildType.IF);
                        thenRegion.setChildType(ChildType.THEN);
                        return true;
                    } else if (Boolean.FALSE.equals(thenEdge.getValue())
                            && Boolean.TRUE.equals(elseEdge.getValue())) {
                        region.setParentType(ParentType.IF_NOT_THEN);
                        ifRegion.setChildType(ChildType.IF);
                        thenRegion.setChildType(ChildType.THEN);
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private boolean checkIfThenElseRegion(Region region) {
        if (region.getChildCount() == 3) {
            Region ifRegion = null;
            Region thenOrElseRegion1 = null;
            Region thenOrElseRegion2 = null;
            for (Region child : region.getChildren()) {
                if (child.getEntry().equals(region.getEntry())) {
                    ifRegion = child;
                } else {
                    if (thenOrElseRegion1 == null) {
                        thenOrElseRegion1 = child;
                    } else {
                        thenOrElseRegion2 = child;
                    }
                }
            }
            Region thenRegion = null;
            Region elseRegion = null;
            if (ifRegion != null && thenOrElseRegion1 != null
                    && thenOrElseRegion2 != null) {
                BasicBlock ifBB = ifRegion.getEntry();
                BasicBlock thenOrElseBB1 = thenOrElseRegion1.getEntry();
                BasicBlock thenOrElseBB2 = thenOrElseRegion2.getEntry();
                Edge thenOrElseEdge1 = cfg.getEdge(ifBB, thenOrElseBB1);
                Edge thenOrElseEdge2 = cfg.getEdge(ifBB, thenOrElseBB2);
                if (thenOrElseEdge1 != null && thenOrElseEdge2 != null) {
                    if (Boolean.TRUE.equals(thenOrElseEdge1.getValue())) {
                        thenRegion = thenOrElseRegion1;
                    } else if (Boolean.TRUE.equals(thenOrElseEdge2.getValue())) {
                        thenRegion = thenOrElseRegion2;
                    }
                    if (Boolean.FALSE.equals(thenOrElseEdge1.getValue())) {
                        elseRegion = thenOrElseRegion1;
                    } else if (Boolean.FALSE.equals(thenOrElseEdge2.getValue())) {
                        elseRegion = thenOrElseRegion2;
                    }
                }
            }
            if (thenRegion != null && elseRegion != null) {
                region.setParentType(ParentType.IF_THEN_ELSE);
                ifRegion.setChildType(ChildType.IF);
                thenRegion.setChildType(ChildType.THEN);
                elseRegion.setChildType(ChildType.ELSE);
                return true;
            }
        }
        return false;
    }

    public void analyse(OutputHandler handler) {
        // build refined program structure tree
        RPST rpst = new RPST(cfg);
        handler.rpstBuilt(rpst);
        for (Region region : rpst.getRegionsPostOrder()) {
            if (region.getParentType() == ParentType.UNDEFINED) {
                if (!(checkIfThenElseRegion(region)
                        || checkIfThenRegion(region)
                        || checkSequenceRegion(region))) {
                    // TODO
                }
            }
        }
        StringBuilder builder = new StringBuilder();
        rpst.print(builder);
        logger.log(Level.FINER, "Region analysis :\n{0}", builder.toString());
    }
}
