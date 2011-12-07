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
package fr.jamgotchian.abcd.core.ir;

import com.google.common.base.Objects;
import fr.jamgotchian.abcd.core.common.ABCDWriter;
import fr.jamgotchian.abcd.core.graph.DominatorInfo;
import fr.jamgotchian.abcd.core.graph.PostDominatorInfo;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RegionAnalysis {

    private static final Logger LOGGER = Logger.getLogger(RegionAnalysis.class.getName());

    private final ControlFlowGraph cfg0;

    private final ABCDWriter writer;

    private int level;

    private int breakLabelCount;

    public RegionAnalysis(ControlFlowGraph cfg0, ABCDWriter writer) {
        this.cfg0 = cfg0;
        this.writer = writer;
    }

    private ControlFlowGraph createSubCFG(ControlFlowGraph cfg, Region region) {
        if (region.getEntry() == null
                || region.getExit() == null
                || region.getEntry().equals(region.getExit())
                || cfg.getPredecessorCountOf(region.getExit()) < 2
                || (region.getEntry().equals(cfg.getEntryBlock())
                    && region.getExit().equals(cfg.getExitBlock()))) {
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
        LOGGER.log(Level.FINEST, "Check trivial region {0}", region);
        if (region.getChildCount() == 1) {
            region.setParentType(ParentType.TRIVIAL);
            return true;
        }
        return false;
    }

    private boolean checkSequenceRegion(Region region) {
        LOGGER.log(Level.FINEST, "Check sequence region {0}", region);
        if (region.getChildCount() != 2) {
            return false;
        }
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
        return false;
    }

    private boolean checkWhileLoopRegion(ControlFlowGraph cfg, Region region) {
        LOGGER.log(Level.FINEST, "Check while loop region {0}", region);
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
                            region.setParentType(ParentType.WHILE_LOOP);
                            headRegion.setChildType(ChildType.LOOP_HEAD);
                            tailRegion.setChildType(ChildType.LOOP_TAIL);
                            return true;
                        } else if (Boolean.FALSE.equals(exitEdge.getValue())
                                && Boolean.TRUE.equals(bodyEdge.getValue())) {
                            region.setParentType(ParentType.WHILE_LOOP_INVERTED_COND);
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

    private boolean checkDoWhileLoopRegion(ControlFlowGraph cfg, Region region) {
        LOGGER.log(Level.FINEST, "Check do while loop region {0}", region);
        if (region.getChildCount() == 1) {
            if (region.getEntry().getType() == BasicBlockType.JUMP_IF) {
                region.setParentType(ParentType.DO_WHILE_LOOP);
                return true;
            }
        }
        return false;
    }

    private boolean checkIfThenRegion(ControlFlowGraph cfg, Region region) {
        LOGGER.log(Level.FINEST, "Check if then region {0}", region);
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
                if (Objects.equal(region.getExit(), thenRegion.getExit())) {
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

    private boolean checkIfThenElseRegion(ControlFlowGraph cfg, Region region) {
        LOGGER.log(Level.FINEST, "Check if then else region {0}", region);
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
            if (thenRegion != null && elseRegion != null
                    && thenRegion.getExit().equals(elseRegion.getExit())) {
                region.setParentType(ParentType.IF_THEN_ELSE);
                ifRegion.setChildType(ChildType.IF);
                thenRegion.setChildType(ChildType.THEN);
                elseRegion.setChildType(ChildType.ELSE);
                return true;
            }
        }
        return false;
    }

    private boolean checkSwitchCaseRegion(ControlFlowGraph cfg, Region region) {
        LOGGER.log(Level.FINEST, "Check switch case region {0}", region);
        if (region.getChildCount() < 2) {
            return false;
        }
        Region switchRegion = null;
        List<Region> caseRegions = new ArrayList<Region>();
        for (Region child : region.getChildren()) {
            if (child.getEntry().equals(region.getEntry())
                    && child.getExit().getType() == BasicBlockType.SWITCH) {
                switchRegion = child;
            } else {
                caseRegions.add(child);
            }
        }
        if (switchRegion == null) {
            return false;
        }
        for (Region caseRegion : caseRegions) {
            Edge incomingEdge
                    = cfg.getEdge(switchRegion.getExit(), caseRegion.getEntry());
            if (incomingEdge == null
                    || !(incomingEdge.getValue() instanceof CaseValues)) {
                return false;
            }
            if (!caseRegion.getExit().equals(region.getExit())) {
                return false;
            }
            caseRegion.setData(incomingEdge.getValue());
        }
        Edge emptyCaseEdge = cfg.getEdge(region.getEntry(), region.getExit());
        if (emptyCaseEdge != null
                && emptyCaseEdge.getValue() instanceof CaseValues) {
            // store empty cases info in switch region
            switchRegion.setData(emptyCaseEdge.getValue());
        }

        region.setParentType(ParentType.SWITCH_CASE);
        switchRegion.setChildType(ChildType.SWITCH);
        for (Region caseRegion : caseRegions) {
            caseRegion.setChildType(ChildType.CASE);
        }
        return true;
    }

    private boolean checkTryCatchFinally(ControlFlowGraph cfg, Region region) {
        LOGGER.log(Level.FINEST, "Check try catch finally region {0}", region);
        Set<Region> handlerRegions = new HashSet<Region>();
        Region finallyRegion = null;
        Set<BasicBlock> handlerEntries = new HashSet<BasicBlock>();
        for (Region child : region.getChildren()) {
            // the child region is an exception handler whether its exit is connected
            // to parent region exit and its entry basic block has attribute
            // EXCEPTION_HANDLER_ENTRY
            if (child.getExit().equals(region.getExit())
                    && child.getEntry().hasProperty(BasicBlockPropertyName.EXCEPTION_HANDLER_ENTRY)) {
                handlerRegions.add(child);
                handlerEntries.add(child.getEntry());
                if (child.getEntry().hasProperty(BasicBlockPropertyName.FINALLY_ENTRY)) {
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
                if (child.equals(finallyRegion)) {
                    continue;
                }
                if (child.getExit().equals(region.getExit())
                        || child.getExit().hasProperty(BasicBlockPropertyName.BREAK_LABEL_EXIT_TARGET)) {
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
                if (!cfg.getExceptionalSuccessorsOf(bb).containsAll(handlerEntries)) {
                    return false;
                }
            }
        }

        // build control flow subgraph
        ControlFlowGraph subCfg = createSubCFG(cfg, region);
        if (subCfg == null) {
            return false;
        }
        // remove handlers basic blocks
        for (Region handlerRegion : handlerRegions) {
            for (BasicBlock bb : handlerRegion.getBasicBlocks()) {
                subCfg.removeBasicBlock(bb);
            }
        }
        // update dominators and post dominators infos
        subCfg.updateDominatorInfo();
        subCfg.updatePostDominatorInfo();

        // build rpst from control flow subgraph
        RPST subRpst = checkRegions(subCfg);
        if (!subRpst.isTyped()) {
            return false;
        }

        region.setParentType(ParentType.TRY_CATCH_FINALLY);
        region.removeChildren();
        Region rootRegion = subRpst.getRootRegion();
        Region tryRegion = rootRegion.getEntryChild();
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
        for (Region child : tryRegion.getSubRegions()) {
            for (Region inlinedFinallyRegion : inlinedFinallyRegions) {
                if (child.getEntry().equals(inlinedFinallyRegion.getEntry())
                        && child.getExit().equals(inlinedFinallyRegion.getExit())) {
                    child.insertParent(new Region(child.getEntry(),
                                                  child.getExit(),
                                                  ParentType.INLINED_FINALLY));
                    break;
                }
            }
        }

        return true;
    }

    private BasicBlock findJoinBlock(ControlFlowGraph subCfg, BasicBlock exitEdgeSource,
                                     BasicBlock abruptEdgeSource) {
        DominatorInfo<BasicBlock, Edge> domInfo = subCfg.getDominatorInfo();
        PostDominatorInfo<BasicBlock, Edge> postDomInfo = subCfg.getPostDominatorInfo();
        for (BasicBlock forkBB = abruptEdgeSource;
                forkBB != null;
                forkBB = domInfo.getDominatorsTree().getParent(forkBB)) {
            if (forkBB.getType() == BasicBlockType.JUMP_IF) {
                Collection<BasicBlock> successors = subCfg.getNormalSuccessorsOf(forkBB);
                Iterator<BasicBlock> itB = successors.iterator();
                BasicBlock bb1 = itB.next();
                BasicBlock bb2 = itB.next();
                if (bb1.equals(subCfg.getExitBlock())) {
                    return bb2;
                } else if (bb2.equals(subCfg.getExitBlock())) {
                    return bb1;
                } else if (postDomInfo.postDominates(exitEdgeSource, bb1)
                        && !postDomInfo.postDominates(exitEdgeSource, bb2)) {
                    return bb1;
                } else if (postDomInfo.postDominates(exitEdgeSource, bb2)
                        && !postDomInfo.postDominates(exitEdgeSource, bb1)) {
                    return bb2;
                }
            }
        }
        return null;
    }

    private Map<Edge, BasicBlock> findJoinBlocks(ControlFlowGraph subCfg, Edge exitEdge,
                                                 List<Edge> abruptEdges) {
        Map<Edge, BasicBlock> joinBlocks = new HashMap<Edge, BasicBlock>();
        BasicBlock exitEdgeSource = subCfg.getEdgeSource(exitEdge);
        for (Edge abruptEdge : abruptEdges) {
            BasicBlock abruptEdgeSource = subCfg.getEdgeSource(abruptEdge);
            BasicBlock joinBlock = findJoinBlock(subCfg, exitEdgeSource, abruptEdgeSource);
            if (joinBlock == null) {
                return null;
            }
            joinBlocks.put(abruptEdge, joinBlock);
        }
        return joinBlocks;
    }

    /**
     * Create smoothed sub control flow graph by replacing abrupt edges by
     * smooth edges
     */
    private ControlFlowGraph createSmoothedSubCfg(ControlFlowGraph subCfg,
                                                  Edge exitEdge,
                                                  Map<Edge, BasicBlock> joinBlocks) {
        ControlFlowGraph cloneSubCfg = subCfg.clone();
        LOGGER.log(Level.FINER, "Exit edge is {0}", cloneSubCfg.toString(exitEdge));
        for (Map.Entry<Edge, BasicBlock> entry : joinBlocks.entrySet()) {
            Edge abruptEdge = entry.getKey();
            BasicBlock joinBlock = entry.getValue();
            LOGGER.log(Level.FINER, "Remove abrupt edge {0} (join block is {1})",
                    new Object[] {cloneSubCfg.toString(abruptEdge), joinBlock});
            BasicBlock source = cloneSubCfg.getEdgeSource(abruptEdge);
            cloneSubCfg.removeEdge(abruptEdge);
            joinBlock.putProperty(BasicBlockPropertyName.BREAK_LABEL_EXIT_TARGET, null);
            if (source.getType() == BasicBlockType.JUMP_IF) {
                BasicBlock empty = new BasicBlockImpl(BasicBlockType.EMPTY);
                cloneSubCfg.addBasicBlock(empty);
                cloneSubCfg.addEdge(source, empty, abruptEdge);
                cloneSubCfg.addEdge(empty, joinBlock).addAttribute(EdgeAttribute.FAKE_EDGE);
                empty.putProperty(BasicBlockPropertyName.BREAK_LABEL_EXIT_SOURCE, breakLabelCount);
            } else {
                cloneSubCfg.addEdge(source, joinBlock, abruptEdge);
                abruptEdge.addAttribute(EdgeAttribute.FAKE_EDGE);
                source.putProperty(BasicBlockPropertyName.BREAK_LABEL_EXIT_SOURCE, breakLabelCount);
            }
            LOGGER.log(Level.FINER, "Add smooth edge {0}", cloneSubCfg.toString(abruptEdge));
        }
        cloneSubCfg.updateDominatorInfo();
        cloneSubCfg.updatePostDominatorInfo();
        cloneSubCfg.updateLoopInfo();
        return cloneSubCfg;
    }

    private boolean checkBreakLabelRegion(ControlFlowGraph cfg, Region region) {
        LOGGER.log(Level.FINEST, "Check break label region {0}", region);
        final ControlFlowGraph subCfg = createSubCFG(cfg, region);
        if (subCfg == null) {
            return false;
        }
        subCfg.updateDominatorInfo();
        subCfg.updatePostDominatorInfo();

        // sort exit edges by dominance of it source basic block
        List<Edge> exitEdges = new ArrayList<Edge>(subCfg.getIncomingEdgesOf(subCfg.getExitBlock()));
        Collections.sort(exitEdges, new Comparator<Edge>() {
            public int compare(Edge e1, Edge e2) {
                BasicBlock bb1 = subCfg.getEdgeSource(e1);
                BasicBlock bb2 = subCfg.getEdgeSource(e2);
                return subCfg.getDominatorInfo().dominates(bb1, bb2) ? 1 : -1;
            }
        });

        for (Edge exitEdge : exitEdges) {
            // the others edges are abrupt edges
            List<Edge> abruptEdges = new ArrayList<Edge>(exitEdges);
            abruptEdges.remove(exitEdge);

            // find join edges, join edges are needed to remove abrupt edges
            Map<Edge, BasicBlock> joinBlocks = findJoinBlocks(subCfg, exitEdge, abruptEdges);
            if (joinBlocks != null) {
                // create a smoothed sub cfg by removing abrupt edges
                ControlFlowGraph smoothedSubCfg
                        = createSmoothedSubCfg(subCfg, exitEdge, joinBlocks);
                RPST rpst = checkRegions(smoothedSubCfg);
                if (rpst.isTyped()) {
                    region.setParentType(ParentType.BREAK_LABEL);
                    region.setData(breakLabelCount++);
                    Region rootRegion = rpst.getRootRegion();
                    Region bodyRegion = rootRegion.getEntryChild();
                    region.removeChildren();
                    region.addChild(bodyRegion);
                    return true;
                }
            }
        }
        return false;
    }

    private RPST checkRegions(ControlFlowGraph cfg) {
        LOGGER.log(Level.FINER, "***** Start checking regions for CFG ({0}, {1}) *****",
                new Object[] {cfg.getEntryBlock(), cfg.getExitBlock()});

        int currentLevel = level++;

        // build refined program structure tree
        RPST rpst = new RPST(cfg);
        rpst.setTyped(true);

        for (Region region : rpst.getRegionsPostOrder()) {
            if (region.getParentType() == ParentType.UNDEFINED) {
                if (!(checkIfThenElseRegion(cfg, region)
                        || checkIfThenRegion(cfg, region)
                        || checkSwitchCaseRegion(cfg, region)
                        || checkSequenceRegion(region)
                        || checkWhileLoopRegion(cfg, region)
                        || checkDoWhileLoopRegion(cfg, region)
                        || checkTrivialRegion(region)
                        || checkTryCatchFinally(cfg, region)
                        || checkBreakLabelRegion(cfg, region))) {
                    rpst.setTyped(false);
                    break;
                } else {
                    LOGGER.log(Level.FINER, "Found {0} region {1}",
                            new Object[] {region.getParentType(), region});
                }
            }
        }

        LOGGER.log(Level.FINER, "***** Analysis {0} for CFG ({1}, {2}) *****",
                new Object[] {rpst.isTyped() ? "succeed" : "failed",
                              cfg.getEntryBlock(), cfg.getExitBlock()});

        writer.writeRPST(rpst, currentLevel);

        return rpst;
    }

    public RPST analyse() {
        level = 0;
        breakLabelCount = 0;
        return checkRegions(cfg0);
    }
}
