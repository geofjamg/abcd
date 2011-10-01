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

import com.google.common.base.Objects;
import fr.jamgotchian.abcd.core.OutputHandler;
import fr.jamgotchian.abcd.core.common.ABCDException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
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

    private ControlFlowGraphImpl createSubCFG(Region region) {
        if (region.getEntry() == null
                || region.getExit() == null
                || region.getEntry().equals(region.getExit())
                || cfg.getPredecessorCountOf(region.getExit()) < 2) {
            return null;
        }
        ControlFlowGraphImpl subCfg
                = new ControlFlowGraphImpl(cfg.getName(), region.getEntry(),
                                           region.getExit());
        for (BasicBlock bb : region.getBasicBlocks()) {
            if (!bb.equals(region.getEntry()) && !bb.equals(region.getExit())) {
                subCfg.addBasicBlock(bb);
            }
        }
        for (Edge e : cfg.getEdges()) {
            BasicBlock source = cfg.getEdgeSource(e);
            BasicBlock target = cfg.getEdgeTarget(e);
            if (subCfg.containsBasicBlock(source)
                    && subCfg.containsBasicBlock(target)) {
                subCfg.addEdge(source, target, e);
            }
        }
        return subCfg;
    }

    private boolean checkTrivialRegion(Region region) {
        logger.log(Level.FINER, "Check trivial region {0}", region);
        if (region.getChildCount() == 1) {
            region.setParentType(ParentType.TRIVIAL);
            return true;
        }
        return false;
    }

    private boolean checkSequenceRegion(Region region) {
        logger.log(Level.FINER, "Check sequence region {0}", region);
        if (region.getChildCount() == 2) {
            Iterator<Region> it = region.getChildren().iterator();
            Region child1 = it.next();
            Region child2 = it.next();
            if (child2.getEntry().equals(child1.getExit())
                    && Objects.equal(region.getExit(), child2.getExit())) {
                region.setParentType(ParentType.SEQUENCE);
                child1.setChildType(ChildType.FIRST);
                child2.setChildType(ChildType.SECOND);
                return true;
            } else if (child1.getEntry().equals(child2.getExit())
                    && Objects.equal(region.getExit(), child1.getExit())) {
                region.setParentType(ParentType.SEQUENCE);
                child2.setChildType(ChildType.FIRST);
                child1.setChildType(ChildType.SECOND);
                return true;
            }
        }
        return false;
    }

    private boolean checkSingleExitLoopRegion(Region region) {
        logger.log(Level.FINER, "Check single exit loop region {0}", region);
        if (region.getChildCount() == 2) {
            Region headRegion = null;
            Region tailRegion = null;
            for (Region child : region.getChildren()) {
                if (child.getEntry().equals(region.getEntry())) {
                    headRegion = child;
                } else {
                    tailRegion = child;
                }
            }
            if (headRegion != null && tailRegion != null) {
                if (tailRegion.getExit().equals(headRegion.getEntry())) {
                    Edge bodyEdge = cfg.getEdge(headRegion.getExit(), tailRegion.getEntry());
                    Edge exitEdge = cfg.getEdge(headRegion.getExit(), region.getExit());
                    if (bodyEdge != null && exitEdge != null
                            && exitEdge.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                        if (Boolean.TRUE.equals(exitEdge.getValue())
                                && Boolean.FALSE.equals(bodyEdge.getValue())) {
                            region.setParentType(ParentType.SINGLE_EXIT_LOOP);
                            headRegion.setChildType(ChildType.LOOP_HEAD);
                            tailRegion.setChildType(ChildType.LOOP_TAIL);
                            return true;
                        } else if (Boolean.FALSE.equals(exitEdge.getValue())
                                && Boolean.TRUE.equals(bodyEdge.getValue())) {
                            region.setParentType(ParentType.SINGLE_EXIT_LOOP_INVERTED_COND);
                            headRegion.setChildType(ChildType.LOOP_HEAD);
                            tailRegion.setChildType(ChildType.LOOP_TAIL);
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    private boolean checkIfThenRegion(Region region) {
        logger.log(Level.FINER, "Check if then region {0}", region);
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
                if (region.getExit().equals(thenRegion.getExit())) {
                    Edge thenEdge = cfg.getEdge(ifRegion.getEntry(), thenRegion.getEntry());
                    Edge elseEdge = cfg.getEdge(ifRegion.getEntry(), region.getExit());
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
        }
        return false;
    }

    private boolean checkIfThenElseRegion(Region region) {
        logger.log(Level.FINER, "Check if then else region {0}", region);
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

    private boolean checkTryCatchFinally(Region region) {
        logger.log(Level.FINER, "Check try catch finally region {0}", region);
        Set<Region> handlerRegions = new HashSet<Region>();
        Set<BasicBlock> handlerEntries = new HashSet<BasicBlock>();
        Region finallyRegion = null;
        for (Region child : region.getChildren()) {
            // the child region is an exception handler whether its exit is connected
            // to parent region exit and if every edges incoming to it entry is
            // exceptional
            boolean isHandler = false;
            boolean isFinally = false;
            if (child.getExit().equals(region.getExit())) {
                if (cfg.getPredecessorCountOf(child.getEntry()) > 0) {
                    isHandler = true;
                    isFinally = true;
                    for (Edge e : cfg.getIncomingEdgesOf(child.getEntry())) {
                        if (!e.isExceptional()) {
                            isHandler = false;
                            break;
                        }
                        ExceptionHandlerInfo info = (ExceptionHandlerInfo) e.getValue();
                        if (info.getClassName() != null) {
                            isFinally = false;
                        }
                    }
                    // skip nested handlers
                    for (Edge e : cfg.getOutgoingEdgesOf(child.getExit())) {
                        if (e.isExceptional()) {
                            isHandler = false;
                            break;
                        }
                    }
                }
            }
            if (isHandler) {
                handlerRegions.add(child);
                handlerEntries.add(child.getEntry());
                if (isFinally) {
                    finallyRegion = child;
                }
            }
        }
        if (handlerRegions.isEmpty()) {
            return false;
        }
        // search inlined finally region
        Set<Region> inlinedFinallyRegions = new HashSet<Region>();
        if (finallyRegion != null) {
            for (Region child : region.getChildren()) {
                if (child.getExit().equals(region.getExit())
                        && !child.equals(finallyRegion)) {
                    if (child.deepEquals(finallyRegion)) {
                        inlinedFinallyRegions.add(child);
                    }
                }
            }
        }
        // every other child region should be connected to handlers
        Set<Region> otherChildren = new HashSet<Region>(region.getChildren());
        otherChildren.removeAll(handlerRegions);
        otherChildren.removeAll(inlinedFinallyRegions);
        for (Region otherChild : otherChildren) {
            for (BasicBlock bb : otherChild.getBasicBlocks()) {
                Set<BasicBlock> handlers = new HashSet<BasicBlock>();
                for (Edge e : cfg.getOutgoingEdgesOf(bb)) {
                    if (e.isExceptional()) {
                        handlers.add(cfg.getEdgeTarget(e));
                    }
                }
                if (!handlers.containsAll(handlerEntries)) {
                    return false;
                }
            }
        }
        // build control flow subgraph
        ControlFlowGraphImpl subCfg = createSubCFG(region);
        if (subCfg == null) {
            return false;
        }
        // remove handlers basic blocks
        for (Region handlerRegion : handlerRegions) {
            for (BasicBlock bb : handlerRegion.getBasicBlocks()) {
                subCfg.removeBasicBlock(bb);
            }
        }
        // remove inlined basic blocks
        for (Region inlinedFinallyRegion : inlinedFinallyRegions) {
            BasicBlock entry = inlinedFinallyRegion.getEntry();
            BasicBlock exit = inlinedFinallyRegion.getExit();
            for (Edge e : new ArrayList<Edge>(subCfg.getIncomingEdgesOf(entry))) {
                BasicBlock p = subCfg.getEdgeSource(e);
                subCfg.removeEdge(e);
                subCfg.addEdge(p, exit, e);
            }
            for (BasicBlock bb : inlinedFinallyRegion.getBasicBlocks()) {
                subCfg.removeBasicBlock(bb);
            }
        }
        // build rpst from control flow subgraph
        subCfg.updateDominatorInfo();
        subCfg.updatePostDominatorInfo();
        RPST subRpst = checkRegions(subCfg);
        region.setParentType(ParentType.TRY_CATCH_FINALLY);
        region.removeChildren();
        Region rootRegion = subRpst.getTopLevelRegion();
        Region tryRegion = rootRegion.getFirstChild();
        tryRegion.setChildType(ChildType.TRY);
        region.addChild(tryRegion);
        for (Region handlerRegion : handlerRegions) {
            if (finallyRegion != null && handlerRegion.equals(finallyRegion)) {
                handlerRegion.setChildType(ChildType.FINALLY);
            } else {
                handlerRegion.setChildType(ChildType.CATCH);
            }
            region.addChild(handlerRegion);
        }
        for (Region inlinedFinallyRegion : inlinedFinallyRegions) {
            region.addChild(inlinedFinallyRegion);
            inlinedFinallyRegion.setChildType(ChildType.INLINED_FINALLY);
        }
        return true;
    }

    private boolean checkBreakLabelRegion(Region region) {
        logger.log(Level.FINER, "Check break label region {0}", region);
        ControlFlowGraphImpl subCfg = createSubCFG(region);
        if (subCfg == null) {
            return false;
        }
        // predecessors of exit ordered by dominance tree depth
        TreeMap<Integer, List<BasicBlock>> breakBB = new TreeMap<Integer, List<BasicBlock>>();
        for (BasicBlock bb : cfg.getPredecessorsOf(subCfg.getExitBlock())) {
            int depth = cfg.getDominatorInfo().getDominatorsTree().getDepthFromRoot(bb);
            if (!breakBB.containsKey(depth)) {
                breakBB.put(depth, new ArrayList<BasicBlock>());
            }
            breakBB.get(depth).add(bb);
        }
        if (breakBB.firstEntry().getValue().size() != 1) {
            throw new ABCDException("firstEntry.getValue().size() != 1");
        }
        breakBB.remove(breakBB.firstKey());
        for (Map.Entry<Integer, List<BasicBlock>> entry : breakBB.entrySet()) {
            for (BasicBlock bb : entry.getValue()) {
                subCfg.removeEdge(bb, subCfg.getExitBlock());
            }
        }
        subCfg.updateDominatorInfo();
        subCfg.updatePostDominatorInfo();
        RPST rpst = checkRegions(subCfg);
        for (Map.Entry<Integer, List<BasicBlock>> entry : breakBB.entrySet()) {
            for (BasicBlock bb : entry.getValue()) {
                if (bb.getType() == BasicBlockType.JUMP_IF) {
                    Edge exitEdge = cfg.getEdge(bb, region.getExit());
                    boolean value = (Boolean) exitEdge.getValue();
                    bb.getParent().setParentType(value ? ParentType.BASIC_BLOCK_IF_THEN_BREAK
                                                       : ParentType.BASIC_BLOCK_IF_NOT_THEN_BREAK);
                } else {
                    throw new ABCDException("TODO");
                }
            }
        }
        region.setParentType(ParentType.BREAK_LABEL);
        Region rootRegion = rpst.getTopLevelRegion();
        Region bodyRegion = rootRegion.getFirstChild();
        region.removeChildren();
        region.addChild(bodyRegion);
        return true;
    }

    private RPST checkRegions(ControlFlowGraph subcfg) {
        // build refined program structure tree
        RPST rpst = new RPST(subcfg);
        for (Region region : rpst.getRegionsPostOrder()) {
            if (region.getParentType() == ParentType.UNDEFINED) {
                if (!(checkTrivialRegion(region)
                        || checkIfThenElseRegion(region)
                        || checkIfThenRegion(region)
                        || checkSequenceRegion(region)
                        || checkSingleExitLoopRegion(region)
                        || checkTryCatchFinally(region)
                        || checkBreakLabelRegion(region))) {
                    throw new ABCDException("Region analysis failed");
                } else {
                    logger.log(Level.FINER, "Found {0} region {1}",
                            new Object[] {region.getParentType(), region});
                }
            }
        }
        return rpst;
    }

    public void analyse(OutputHandler handler) {
        RPST rpst = checkRegions(cfg);
        handler.rpstBuilt(rpst);
        StringBuilder builder = new StringBuilder();
        rpst.print(builder);
        logger.log(Level.FINER, "Region analysis :\n{0}", builder.toString());
    }
}