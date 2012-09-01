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
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.common.ABCDWriter;
import fr.jamgotchian.abcd.core.graph.Filter;
import static fr.jamgotchian.abcd.core.ir.BasicBlockPropertyName.*;
import fr.jamgotchian.abcd.core.ir.RPSTLogger.Log;
import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RegionAnalysis {

    private static final Logger LOGGER = LoggerFactory.getLogger(RegionAnalysis.class.getName());

    private final ControlFlowGraph cfg0;

    private final ABCDWriter writer;

    private RPSTLogger rpstLogger;

    public RegionAnalysis(ControlFlowGraph cfg0, ABCDWriter writer) {
        this.cfg0 = cfg0;
        this.writer = writer;
    }

    private static ControlFlowGraph createSubCFG(RPST rpst, Region region) {
        if (region.getEntry() == null
                || region.getExit() == null
                || region.getEntry().equals(region.getExit())) {
            throw new ABCDException("Cannot create subgraph from region " + region);
        }
        ControlFlowGraph cfg = rpst.getCfg();
        ControlFlowGraph subCfg
                = new ControlFlowGraph("Subgraph of " + cfg.getName(),
                                       region.getEntry(), region.getExit(),
                                       cfg.getBytecodeRenderer());
        for (BasicBlock bb : rpst.getBasicBlocks(region)) {
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

    private boolean checkTrivialRegion(RPST rpst, Region region) {
        LOGGER.trace("Check trivial region {}", region);
        if (rpst.getChildCount(region) == 1) {
            Region child = rpst.getFirstChild(region);
            if (child.getEntry().getType() != BasicBlockType.JUMP_IF) {
                LOGGER.debug("Found trivial region {}", region);
                region.setParentType(ParentType.TRIVIAL);

                // propagate to children
                if (!checkRegion(rpst, child)) {
                    throw new ABCDException("Cannot find type of region "
                            + child);
                }
                return true;
            }
        }
        return false;
    }

    private boolean checkSequenceRegion(RPST rpst, Region region) {
        LOGGER.trace("Check sequence region {}", region);
        if (rpst.getChildCount(region) != 2) {
            return false;
        }
        Iterator<Region> it = rpst.getChildren(region).iterator();
        Region child1 = it.next();
        Region child2 = it.next();
        Region firstRegion = null;
        Region secondRegion = null;
        if (child2.getEntry().equals(child1.getExit())
                && Objects.equal(region.getExit(), child2.getExit())) {
            firstRegion = child1;
            secondRegion = child2;
        } else if (child1.getEntry().equals(child2.getExit())
                && Objects.equal(region.getExit(), child1.getExit())) {
            firstRegion = child2;
            secondRegion = child1;
        }
        if (firstRegion != null && secondRegion != null) {
            LOGGER.debug("Found sequence region {}", region);
            region.setParentType(ParentType.SEQUENCE);
            firstRegion.setChildType(ChildType.FIRST);
            secondRegion.setChildType(ChildType.SECOND);

            // propagate to children
            if (!checkRegion(rpst, firstRegion)) {
                throw new ABCDException("Cannot find type of first region "
                            + firstRegion);
            }
            if (!checkRegion(rpst, secondRegion)) {
                throw new ABCDException("Cannot find type of second region "
                            + secondRegion);
            }
            return true;
        }

        return false;
    }

    private boolean checkWhileLoopRegion(RPST rpst, Region region) {
        LOGGER.trace("Check while loop region {}", region);
        // a loop region must have 2 incomings edges, the region entry edge
        // and the loop back edge
        if (rpst.getCfg().getPredecessorCountOf(region.getEntry()) != 2) {
            return false;
        }
        // check that one of the 2 incoming edges is a back edge
        Edge backEdge = null;
        for (Edge e : rpst.getCfg().getIncomingEdgesOf(region.getEntry())) {
            if (e.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
                backEdge = e;
            } else if (e.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
                backEdge = e;
            }
        }
        if (backEdge == null) {
            return false;
        }
        // check that the loop region also contains the tail block
        BasicBlock tailBlock = rpst.getCfg().getEdgeSource(backEdge);
        if (!rpst.getBasicBlocks(region).contains(tailBlock)) {
            return false;
        }

        LOGGER.debug("Found while loop region {}", region);
        region.setParentType(ParentType.WHILE_LOOP);

        // build the body subgraph
        //   - remove exit edges and exit block
        //   - replace the exit block by the tail block
        //   - remove the back edge
        ControlFlowGraph subCfg = createSubCFG(rpst, region);
        for (Edge exitEdge : new ArrayList<Edge>(subCfg.getIncomingEdgesOf(subCfg.getExitBlock()))) {
            BasicBlock source = subCfg.getEdgeSource(exitEdge);
            BasicBlock _break = BasicBlockImpl.createBreak();
            subCfg.addBasicBlock(_break);
            subCfg.removeEdge(exitEdge);
            subCfg.addEdge(source, _break, exitEdge);
        }
        subCfg.removeBasicBlock(subCfg.getExitBlock());
        subCfg.setExitBlock(tailBlock);
        subCfg.removeEdge(backEdge);

        // build rpst from control flow subgraph
        RPST subRpst = checkGraph(subCfg, "Body subgraph of loop " + region);

        // replace the subtree
        Region bodyRegion1 = subRpst.getChildWithEntry(subRpst.getRootRegion(), subCfg.getEntryBlock());
        Region bodyRegion2 = subRpst.getChildWithEntry(subRpst.getRootRegion(), subCfg.getExitBlock());
        Region bodyRegion = new RegionImpl(region.getEntry(), region.getExit(), ParentType.SEQUENCE);
        bodyRegion1.setChildType(ChildType.FIRST);
        bodyRegion2.setChildType(ChildType.SECOND);
        bodyRegion.setChildType(ChildType.LOOP_BODY);
        rpst.removeChildren(region);
        rpst.addRegion(bodyRegion, region);
        rpst.addRPST(subRpst, bodyRegion1, bodyRegion);
        rpst.addRPST(subRpst, bodyRegion2, bodyRegion);

        return true;
    }

    private boolean checkDoWhileLoopRegion(RPST rpst, Region region) {
        LOGGER.trace("Check do while loop region {}", region);
        if (rpst.getChildCount(region) == 1) {
            if (region.getEntry().getType() == BasicBlockType.JUMP_IF) {
                if (rpst.getCfg().getSuccessorCountOf(region.getEntry()) == 2) {
                    Region bodyRegion = rpst.getEntryChild(region);

                    LOGGER.debug("Found do while loop region {}", region);

                    region.setParentType(ParentType.DO_WHILE_LOOP);
                    bodyRegion.setChildType(ChildType.LOOP_BODY);

                    // propagate to children
                    if (!checkRegion(rpst, bodyRegion)) {
                        throw new ABCDException("Cannot find type of body region "
                                + bodyRegion);
                    }
                    return true;
                }
            }
        }
        return false;
    }

    private boolean checkIfThenRegion(RPST rpst, Region region) {
        LOGGER.trace("Check if then region {}", region);
        if (rpst.getChildCount(region) == 2) {
            Region ifRegion = null;
            Region thenRegion = null;
            for (Region child : rpst.getChildren(region)) {
                if (child.getEntry().equals(region.getEntry())) {
                    ifRegion = child;
                } else {
                    thenRegion = child;
                }
            }
            if (ifRegion != null && thenRegion != null) {
                if (Objects.equal(region.getExit(), thenRegion.getExit())) {
                    Edge thenEdge = rpst.getCfg().getEdge(ifRegion.getEntry(), thenRegion.getEntry());
                    Edge elseEdge = rpst.getCfg().getEdge(ifRegion.getEntry(), region.getExit());
                    if (thenEdge != null && elseEdge != null) {
                        ParentType parentType = null;
                        if (Boolean.TRUE.equals(thenEdge.getValue())
                                && Boolean.FALSE.equals(elseEdge.getValue())) {
                            parentType = ParentType.IF_THEN;
                        } else if (Boolean.FALSE.equals(thenEdge.getValue())
                                && Boolean.TRUE.equals(elseEdge.getValue())) {
                            parentType = ParentType.IF_NOT_THEN;
                        }
                        if (parentType != null) {
                            LOGGER.debug("Found if then region {}", region);
                            region.setParentType(parentType);
                            ifRegion.setChildType(ChildType.IF);
                            thenRegion.setChildType(ChildType.THEN);

                            // propagate to children
                            if (!checkRegion(rpst, ifRegion)) {
                                throw new ABCDException("Cannot find type of if region "
                                        + ifRegion);
                            }
                            if (!checkRegion(rpst, thenRegion)) {
                                throw new ABCDException("Cannot find type of then region "
                                        + thenRegion);
                            }
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    private boolean checkIfThenElseRegion(RPST rpst, Region region) {
        LOGGER.trace("Check if then else region {}", region);
        if (rpst.getChildCount(region) == 3) {
            Region ifRegion = null;
            Region thenOrElseRegion1 = null;
            Region thenOrElseRegion2 = null;
            for (Region child : rpst.getChildren(region)) {
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
                Edge thenOrElseEdge1 = rpst.getCfg().getEdge(ifBB, thenOrElseBB1);
                Edge thenOrElseEdge2 = rpst.getCfg().getEdge(ifBB, thenOrElseBB2);
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
                LOGGER.debug("Found if then else region {}", region);
                region.setParentType(ParentType.IF_THEN_ELSE);
                ifRegion.setChildType(ChildType.IF);
                thenRegion.setChildType(ChildType.THEN);
                elseRegion.setChildType(ChildType.ELSE);

                // propagate to children
                if (!checkRegion(rpst, ifRegion)) {
                    throw new ABCDException("Cannot find type of if region "
                            + ifRegion);
                }
                if (!checkRegion(rpst, thenRegion)) {
                    throw new ABCDException("Cannot find type of then region "
                            + thenRegion);
                }
                if (!checkRegion(rpst, elseRegion)) {
                    throw new ABCDException("Cannot find type of else region "
                            + elseRegion);
                }
                return true;
            }
        }
        return false;
    }

    private boolean checkSwitchCaseRegion(RPST rpst, Region region) {
        LOGGER.trace("Check switch case region {}", region);
        if (rpst.getChildCount(region) < 2) {
            return false;
        }
        Region switchRegion = null;
        List<Region> caseRegions = new ArrayList<Region>();
        for (Region child : rpst.getChildren(region)) {
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
                    = rpst.getCfg().getEdge(switchRegion.getExit(), caseRegion.getEntry());
            if (incomingEdge == null
                    || !(incomingEdge.getValue() instanceof CaseValues)) {
                return false;
            }
            if (!caseRegion.getExit().equals(region.getExit())) {
                return false;
            }
            caseRegion.setData(incomingEdge.getValue());
        }
        Edge emptyCaseEdge = rpst.getCfg().getEdge(region.getEntry(), region.getExit());
        if (emptyCaseEdge != null
                && emptyCaseEdge.getValue() instanceof CaseValues) {
            // store empty cases info in switch region
            switchRegion.setData(emptyCaseEdge.getValue());
        }

        LOGGER.debug("Found switch case region {}", region);
        region.setParentType(ParentType.SWITCH_CASE);
        switchRegion.setChildType(ChildType.SWITCH);
        for (Region caseRegion : caseRegions) {
            caseRegion.setChildType(ChildType.CASE);
        }

        // propagate to children
        if (!checkRegion(rpst, switchRegion)) {
            throw new ABCDException("Cannot find type of switch region "
                            + switchRegion);
        }
        for (Region caseRegion : caseRegions) {
            if (!checkRegion(rpst, caseRegion)) {
                throw new ABCDException("Cannot find type of case region "
                            + caseRegion);
            }
        }
        return true;
    }

    private boolean checkTryFinallyRegion(final RPST rpst, final Region region) {
        LOGGER.trace("Check try finally region {}", region);

        final ControlFlowGraph cfg = rpst.getCfg();

        //
        // find finally region
        //
        Set<Region> finallyRegions = rpst.getChildren(region, new Filter<Region>() {
            @Override
            public boolean accept(Region r) {
                return r.getEntry().hasProperty(FINALLY_ENTRY)
                        && r.getExit() == region.getExit();
            }
        });
        if (finallyRegions.size() != 1) {
            return false;
        }
        final Region finallyRegion = finallyRegions.iterator().next();
        LOGGER.info("finallyRegion=" + finallyRegion.toString());

        //
        // find try regions
        //
        Set<Region> tryRegions = rpst.getChildren(region, new Filter<Region>() {
            @Override
            public boolean accept(Region r) {
                if (r == finallyRegion) {
                    return false;
                }
                for (BasicBlock bb : rpst.getBasicBlocks(r)) {
                    if (!cfg.getExceptionalSuccessorsOf(bb).contains(finallyRegion.getEntry())) {
                        return false;
                    }
                }
                return true;
            }
        });
        if (tryRegions.isEmpty()) {
            return false;
        }
        LOGGER.info("tryRegions=" + tryRegions.toString());

        //
        // find try blocks
        //
        Set<BasicBlock> tryBlocks = new HashSet<BasicBlock>();
        for (Region tryRegion : tryRegions) {
            tryBlocks.addAll(rpst.getBasicBlocks(tryRegion));
        }

        LOGGER.info("tryBlocks=" + tryBlocks.toString());

        //
        // find try exit blocks
        //
        final Set<BasicBlock> tryExitBlocks = new HashSet<BasicBlock>();
        final Set<Edge> tryExitEdges = new HashSet<Edge>();
        for (BasicBlock tryBlock : tryBlocks) {
            for (Edge e : cfg.getNormalOutgoingEdgesOf(tryBlock)) {
                if (!e.hasAttribute(EdgeAttribute.THROW_FAKE_EDGE)) {
                    BasicBlock t = cfg.getEdgeTarget(e);
                    if (!tryBlocks.contains(t)) {
                        tryExitEdges.add(e);
                        tryExitBlocks.add(t);
                    }
                }
            }
        }
        if (tryExitBlocks.isEmpty()) {
            return false;
        }
        LOGGER.info("tryExitEdges=" + cfg.toString(tryExitEdges));
        LOGGER.info("tryExitBlocks=" + tryExitBlocks.toString());

        //
        // type children
        //
        for (Region child : rpst.getChildren(region)) {
            if (!checkRegion(rpst, child)) {
                throw new ABCDException("Cannot find type of try catch child region "
                            + child);
            }
        }

        //
        // find inlined regions
        //
        Set<Region> inlinedRegions = rpst.getSubTreeRegions(region, new Filter<Region>() {
            @Override
            public boolean accept(Region r) {
                return tryExitBlocks.contains(r.getEntry())
                        && Regions.deepEquals(rpst, r, rpst, finallyRegion);
            }
        });
        LOGGER.info("inlinedRegions=" + inlinedRegions.toString());
        if (inlinedRegions.size() != tryExitBlocks.size()) {
            return false;
        }

        //
        // find main exit region and break regions
        //
        Set<Region> breakExitRegions = new HashSet<Region>();
        Region mainExitRegion = null;
        Region mainInlinedRegion = null;
        for (final Region inlinedRegion : inlinedRegions) {
            if (inlinedRegion.getExit() != region.getExit()) {
                Set<Region> exitRegions = rpst.getSubTreeRegions(region, new Filter<Region>() {
                    @Override
                    public boolean accept(Region r) {
                        return inlinedRegion.getExit() == r.getEntry()
                                && r.getExit() == region.getExit();
                    }
                });
                if (exitRegions.size() != 1) {
                    return false;
                }
                Region exitRegion = exitRegions.iterator().next();
                if (exitRegion.getEntry().getType() == BasicBlockType.BREAK) {
                    breakExitRegions.add(exitRegion);
                } else {
                    if (mainExitRegion != null) {
                        return false;
                    }
                    mainExitRegion = exitRegion;
                    mainInlinedRegion = inlinedRegion;
                }
            }
        }
        LOGGER.info("breakExitRegions=" + breakExitRegions.toString());
        LOGGER.info("mainExitRegion=" + mainExitRegion);
        LOGGER.info("mainInlinedRegion=" + mainInlinedRegion);

        // build control flow subgraph
        ControlFlowGraph subCfg = createSubCFG(rpst, region);

        if (mainExitRegion != null) {
            subCfg.removeBasicBlock(subCfg.getExitBlock());
            subCfg.setExitBlock(mainExitRegion.getEntry());
        }

        for (Edge e : tryExitEdges) {
            BasicBlock s = subCfg.getEdgeSource(e);
            subCfg.removeEdge(e);
            subCfg.addEdge(s, subCfg.getExitBlock(), e);
        }

        // remove finally basic blocks
        List<Region> toRemove = new ArrayList<Region>();
        toRemove.add(finallyRegion); // remove finally region
        toRemove.addAll(inlinedRegions); // remove inlined regions
        toRemove.addAll(breakExitRegions); // remove break exit regions
        for (Region r : toRemove) {
            for (BasicBlock bb : rpst.getBasicBlocks(r)) {
                subCfg.removeBasicBlock(bb);
            }
        }
        if (mainExitRegion != null) {
            for (BasicBlock bb : rpst.getBasicBlocks(mainExitRegion)) {
                if (bb != mainExitRegion.getEntry()) {
                    subCfg.removeBasicBlock(bb);
                }
            }
        }

        // build rpst from control flow subgraph
        RPST subRpst = checkGraph(subCfg, "Try subgraph of region " + region);

        LOGGER.debug("Found try finally region {}", region);

        RPST tmpRpst = new RPST(cfg, new RegionImpl(null, null, ParentType.ROOT));
        Region tryFinallyRegion = new RegionImpl(region.getEntry(), region.getExit(), ParentType.TRY_CATCH_FINALLY);
        tmpRpst.addRegion(tryFinallyRegion, tmpRpst.getRootRegion());

        tmpRpst.addRPST(rpst, finallyRegion, tryFinallyRegion);
        finallyRegion.setChildType(ChildType.FINALLY);

        for (Region inlinedRegion : inlinedRegions) {
            tmpRpst.addRPST(rpst, inlinedRegion, tryFinallyRegion);
            inlinedRegion.setChildType(ChildType.INLINED);
        }
        for (Region breakExitRegion : breakExitRegions) {
            tmpRpst.addRPST(rpst, breakExitRegion, tryFinallyRegion);
            breakExitRegion.setChildType(ChildType.INLINED);
        }

        Region rootRegion = subRpst.getRootRegion();
        Region tryRegion = subRpst.getEntryChild(rootRegion);
        tryRegion.setChildType(ChildType.TRY);
        tmpRpst.addRPST(subRpst, tryRegion, tryFinallyRegion);

        if (mainExitRegion != null) {
            tmpRpst.addRPST(rpst, mainExitRegion, tmpRpst.getRootRegion());
        }

        rpst.removeChildren(region);

        if (mainExitRegion == null) {
            region.setParentType(ParentType.TRIVIAL);
            rpst.addRPST(tmpRpst, tmpRpst.getRootRegion(), region);
        } else {
            region.setParentType(ParentType.SEQUENCE);
            rpst.addRPST(tmpRpst, tryFinallyRegion, region);
            tryFinallyRegion.setChildType(ChildType.FIRST);
            rpst.addRPST(tmpRpst, mainExitRegion, region);
            mainExitRegion.setChildType(ChildType.SECOND);
        }

        return true;
    }

    private boolean checkTryCatchFinallyRegion(RPST rpst, Region region) {
        LOGGER.trace("Check try catch finally region {}", region);
        Set<Region> exitRegions = rpst.getChildrenWithExit(region, region.getExit());
        Set<Region> handlerRegions = new HashSet<Region>();
        Set<BasicBlock> handlerEntries = new HashSet<BasicBlock>();
        Set<Region> finallyRegions = new HashSet<Region>();
        Set<Region> normalExitRegions = new HashSet<Region>(exitRegions);
        for (Region exitRegion : exitRegions) {
            // the child region is an exception handler whether its exit is connected
            // to parent region exit and its entry basic block has attribute
            // EXCEPTION_HANDLER_ENTRY
            if (exitRegion.getEntry().hasProperty(EXCEPTION_HANDLER_ENTRY)) {
                handlerRegions.add(exitRegion);
                handlerEntries.add(exitRegion.getEntry());
                if (exitRegion.getEntry().hasProperty(FINALLY_ENTRY)) {
                    finallyRegions.add(exitRegion);
                }
            } else {
                normalExitRegions.add(exitRegion);
            }
        }
        if (handlerRegions.isEmpty()) {
            return false;
        }
        if (finallyRegions.size() > 1) {
            throw new ABCDException("Try catch finally region with multiple finally");
        }
        Region finallyRegion = null;
        if (finallyRegions.size() == 1) {
            finallyRegion = finallyRegions.iterator().next();
        }
        // find child regions types (including handlers)
        for (Region child : exitRegions) {
            if (!checkRegion(rpst, child)) {
                throw new ABCDException("Cannot find type of try catch child region "
                            + child);
            }
        }

        // search inlined finally region
        Set<Region> inlinedFinallyRegions = new HashSet<Region>();
        if (finallyRegion != null) {
            for (Region exitRegion : normalExitRegions) {
                if (Regions.deepEquals(rpst, exitRegion, rpst, finallyRegion)) {
                    inlinedFinallyRegions.add(exitRegion);
                } else {
                    if (exitRegion.getParentType() == ParentType.SEQUENCE) {
                        Region grandChild1 = rpst.getFirstChild(exitRegion, ChildType.FIRST);
                        Region grandChild2 = rpst.getFirstChild(exitRegion, ChildType.SECOND);
                        if (grandChild2.getEntry().getType() == BasicBlockType.BREAK
                                && Regions.deepEquals(rpst, grandChild1, rpst, finallyRegion)) {
                            inlinedFinallyRegions.add(exitRegion);
                        }
                    }
                }
            }
        }
        // every other child region should be connected to handlers
        Set<Region> otherChildren = new HashSet<Region>(rpst.getChildren(region));
        otherChildren.removeAll(handlerRegions);
        otherChildren.removeAll(inlinedFinallyRegions);
        for (Region otherChild : otherChildren) {
            for (BasicBlock bb : rpst.getBasicBlocks(otherChild)) {
                if (!rpst.getCfg().getExceptionalSuccessorsOf(bb).containsAll(handlerEntries)) {
                    return false;
                }
            }
        }

        // build control flow subgraph
        ControlFlowGraph subCfg = createSubCFG(rpst, region);

        // remove handlers basic blocks
        for (Region handlerRegion : handlerRegions) {
            for (BasicBlock bb : rpst.getBasicBlocks(handlerRegion)) {
                subCfg.removeBasicBlock(bb);
            }
        }

        // build rpst from control flow subgraph
        RPST subRpst = checkGraph(subCfg, "Try subgraph of region " + region);

        LOGGER.debug("Found try catch finally region {}", region);

        region.setParentType(ParentType.TRY_CATCH_FINALLY);
        for (Region child : new ArrayList<Region>(rpst.getChildren(region))) {
            if (!handlerRegions.contains(child)) {
                rpst.removeSubtree(child);
            }
        }
        Region rootRegion = subRpst.getRootRegion();
        Region tryRegion = subRpst.getEntryChild(rootRegion);
        tryRegion.setChildType(ChildType.TRY);
        rpst.addRPST(subRpst, tryRegion, region);
        for (Region handlerRegion : handlerRegions) {
            if (finallyRegion != null && handlerRegion.equals(finallyRegion)) {
                handlerRegion.setChildType(ChildType.FINALLY);
            } else {
                handlerRegion.setChildType(ChildType.CATCH);
            }
        }
        // insert inlined finally regions
        for (Region child : rpst.getSubTreeRegions(tryRegion)) {
            for (Region inlinedFinallyRegion : inlinedFinallyRegions) {
                if (child.getEntry().equals(inlinedFinallyRegion.getEntry())
                        && child.getExit().equals(inlinedFinallyRegion.getExit())) {
                    rpst.insertRegion(new RegionImpl(child.getEntry(),
                                                     child.getExit(),
                                                     ParentType.INLINED_FINALLY),
                                      child);
                    break;
                }
            }
        }

        return true;
    }

    private boolean checkBreakLabelRegion(RPST rpst, Region region) {
        LOGGER.trace("Check break label region {}", region);
        Collection<Region> exitChildren = rpst.getChildrenWithExit(region, region.getExit());
        Set<Region> joinRegions = new HashSet<Region>();
        for (Region exitChild : exitChildren) {
            if (rpst.getCfg().getNormalPredecessorCountOf(exitChild.getEntry()) > 1) {
                joinRegions.add(exitChild);
            }
        }
        if (joinRegions.size() != 1) {
            return false;
        }
        Region joinRegion = joinRegions.iterator().next();
        BasicBlock joinBlock = joinRegion.getEntry();
        ControlFlowGraph subCfg = createSubCFG(rpst, region);
        BasicBlock newExit = BasicBlockImpl.createExit();
        subCfg.addBasicBlock(newExit);
        for (Edge joinEdge : new ArrayList<Edge>(subCfg.getIncomingEdgesOf(joinBlock))) {
            BasicBlock source = subCfg.getEdgeSource(joinEdge);
            subCfg.removeEdge(joinEdge);
            subCfg.addEdge(source, newExit, joinEdge);
        }
        for (BasicBlock bb : rpst.getBasicBlocks(joinRegion)) {
            subCfg.removeBasicBlock(bb);
        }
        for (Edge exitEdge : new ArrayList<Edge>(subCfg.getIncomingEdgesOf(subCfg.getExitBlock()))) {
            BasicBlock source = subCfg.getEdgeSource(exitEdge);
            subCfg.removeEdge(exitEdge);
            subCfg.addEdge(source, newExit, exitEdge);
            exitEdge.addAttribute(EdgeAttribute.BREAK_FAKE_EDGE);
        }
        subCfg.removeBasicBlock(subCfg.getExitBlock());
        subCfg.setExitBlock(newExit);

        RPST subRpst = checkGraph(subCfg, "Break label subgraph of region " + region);

        LOGGER.debug("Found break label region {}", region);

        region.setParentType(ParentType.BREAK_LABEL);
        Region rootRegion = subRpst.getRootRegion();
        Region forkRegion = subRpst.getEntryChild(rootRegion);
        forkRegion.setChildType(ChildType.FIRST);
        joinRegion.setChildType(ChildType.SECOND);
        for (Region child : new ArrayList<Region>(rpst.getChildren(region))) {
            if (!child.equals(joinRegion)) {
                rpst.removeSubtree(child);
            }
        }
        rpst.addRPST(subRpst, forkRegion, region);

        if (!checkRegion(rpst, joinRegion)) {
            throw new ABCDException("Cannot find type of join region "
                        + joinRegion);
        }

        return true;
    }

    private boolean checkRegion(RPST rpst, Region region) {
        if (region.getParentType() != ParentType.UNDEFINED) {
            return true;
        }
        if (!(checkIfThenElseRegion(rpst, region)
                || checkIfThenRegion(rpst, region)
                || checkSwitchCaseRegion(rpst, region)
                || checkSequenceRegion(rpst, region)
                || checkWhileLoopRegion(rpst, region)
                || checkDoWhileLoopRegion(rpst, region)
                || checkTrivialRegion(rpst, region)
                || checkTryFinallyRegion(rpst, region)
                || checkBreakLabelRegion(rpst, region))) {
            return false;
        } else {
            return true;
        }
    }

    private void checkRegions(RPST rpst) {
        Region root = rpst.getRootRegion();
        for (Region child : rpst.getChildren(root)) {
            if (!checkRegion(rpst, child)) {
                throw new ABCDException("Cannot find type of top level region "
                        + child);
            }
        }
    }

    private RPST checkGraph(ControlFlowGraph cfg, String logTitle) {
        LOGGER.debug("@@@ Check {}", logTitle);

        Log log = rpstLogger.newLog(logTitle);

        cfg.updateDominatorInfo();
        cfg.updateLoopInfo();
        cfg.ensureSingleExit();
        cfg.updatePostDominatorInfo();

        log.setCfg(cfg);

        RPST rpst = new RPSTBuilder(cfg).build();

        log.setRpst(rpst);

        checkRegions(rpst);

        return rpst;
    }

    public RPST analyse() {
        rpstLogger = new RPSTLogger(cfg0.getName());

        cfg0.removeCriticalEdges();

        ControlFlowGraph cfg = new ControlFlowGraph(cfg0);

        RPST rpst;
        try {
            rpst = checkGraph(cfg, "Main graph");
        } finally {
            writer.writeRPST(rpstLogger);
        }

        return rpst;
    }
}
