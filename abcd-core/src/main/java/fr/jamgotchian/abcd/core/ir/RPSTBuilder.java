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

import fr.jamgotchian.abcd.core.graph.PostDominatorInfo;
import fr.jamgotchian.abcd.core.graph.DominatorInfo;
import fr.jamgotchian.abcd.core.util.Sets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Build a refined program structure tree (RPST) from control flow graph.
 *
 * A RPST is a tree build out of canonical refined regions of a function.
 *
 * This is an implementation of the algorithm described in the following paper :
 * The refined program structure tree, Calculate a fine grained PST taking
 * advantage of dominance information, Tobias Grosser, University of Passau,
 * May 19, 2010.
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RPSTBuilder {

    private static final Logger LOGGER = LoggerFactory.getLogger(RPSTBuilder.class);

    private final ControlFlowGraph cfg;

    private final Map<BasicBlock, Region> bb2region = new HashMap<BasicBlock, Region>();

    public RPSTBuilder(ControlFlowGraph cfg) {
        this.cfg = cfg;
    }

    private DominatorInfo<BasicBlock, Edge> getDomInfo() {
        return cfg.getDominatorInfo();
    }

    private PostDominatorInfo<BasicBlock, Edge> getPostDomInfo() {
        return cfg.getPostDominatorInfo();
    }

    private boolean isCommonDomFrontier(BasicBlock bb, BasicBlock entry, BasicBlock exit) {
        for (BasicBlock p : cfg.getPredecessorsOf(bb)) {
            if (getDomInfo().dominates(entry, p) && !getDomInfo().dominates(exit, p)) {
                return false;
            }
        }
        return true;
    }

    private boolean isRegion(BasicBlock entry, BasicBlock exit) {
        if (!getDomInfo().dominates(entry, exit)) {
            if (!Sets.isSubset(getDomInfo().getDominanceFrontierOf2(entry),
                               Collections.singleton(exit))) {
                return false;
            }
        } else {
            if (!Sets.isSubset(getDomInfo().getDominanceFrontierOf2(entry),
                               com.google.common.collect.Sets.union(getDomInfo().getDominanceFrontierOf2(exit),
                                          Collections.singleton(entry)))) {
                return false;
            }
            for (BasicBlock bb : getDomInfo().getDominanceFrontierOf2(entry)) {
                if (!isCommonDomFrontier(bb, entry, exit)) {
                    return false;
                }
            }
            for (BasicBlock bb : getDomInfo().getDominanceFrontierOf2(exit)) {
                if (getDomInfo().strictlyDominates(entry, bb)) {
                    return false;
                }
            }
        }
        return true;
    }

    private void insertShortCut(BasicBlock entry, BasicBlock exit,
                                Map<BasicBlock, BasicBlock> shortCut) {
        assert entry != null && exit != null;
        BasicBlock bb = shortCut.get(exit);
        if (bb == null) {
            shortCut.put(entry, exit);
        } else {
            shortCut.put(entry, bb);
        }
    }

    private BasicBlock getNextPostDom(BasicBlock bb, Map<BasicBlock, BasicBlock> shortCut) {
        BasicBlock bb2 = shortCut.get(bb);
        if (bb2 == null) {
            return getPostDomInfo().getImmediatePostDominatorOf(bb);
        } else {
            return getPostDomInfo().getImmediatePostDominatorOf(bb2);
        }
    }

    private Region createRegion(BasicBlock entry, BasicBlock exit) {
        assert entry != null && exit != null;
        Region newRegion = new Region(entry, exit, ParentType.UNDEFINED);
        if (!bb2region.containsKey(entry)) {
            bb2region.put(entry, newRegion);
        }
        LOGGER.debug("New Region {}", newRegion);
        return newRegion;
    }

    private void detectRegionsWithEntry(BasicBlock entry, Map<BasicBlock, BasicBlock> shortCut) {
        assert  entry != null;
        BasicBlock exit = entry;
        Region lastRegion = null;
        BasicBlock lastExit = entry;
        while ((exit = getNextPostDom(exit, shortCut)) != null) {
            if (isRegion(entry, exit)) {
                Region newRegion = createRegion(entry, exit);
                if (lastRegion != null) {
                    lastRegion.setParent(newRegion);
                     LOGGER.trace("Parent of region {} is {}", lastRegion, newRegion);
                } else {
                    entry.setRegion(newRegion);
                    LOGGER.trace("Parent of BB {} is {}", entry, newRegion);
                }
                lastRegion = newRegion;
                lastExit = exit;
            }
            if (!getDomInfo().dominates(entry, exit)) {
                break;
            }
        }

        if (!lastExit.equals(entry)) {
            insertShortCut(entry, lastExit, shortCut);
        }
    }

    private void detectRegions(Map<BasicBlock, BasicBlock> shortCut) {
        for (BasicBlock bb : getDomInfo().getDominatorsTree().getNodesPostOrder()) {
            detectRegionsWithEntry(bb, shortCut);
        }
    }

    private Region getTopMostParent(Region region) {
        while (region.getParent() != null) {
            region = region.getParent();
        }
        return region;
    }

    private void buildRegionTree(BasicBlock bb, Region region) {
        while (bb.equals(region.getExit())) {
            region = region.getParent();
        }
        Region newRegion = bb2region.get(bb);
        if (newRegion != null) {
            Region topMostParent = getTopMostParent(newRegion);
            LOGGER.trace("Parent of region {} is {}", topMostParent, region);
            topMostParent.setParent(region);
            region = newRegion;
        } else {
            LOGGER.trace("Parent of BB {} is {}", bb, region);
            bb.setRegion(region);
            bb2region.put(bb, region);
        }
        for (BasicBlock c : getDomInfo().getDominatorsTree().getChildren(bb)) {
            buildRegionTree(c, region);
        }
    }

    /**
     * Test if two canonical regions could be merge in one non canonical region.
     */
    private boolean isNonCanonicalRegion(Region region1, Region region2) {
        if (!region1.getExit().equals(region2.getEntry())) {
            return false;
        }
        if (region2.getChildCount() == 0) {
            return false;
        }
        // basic blocks of merged region
        Set<BasicBlock> basicBlocks = new HashSet<BasicBlock>();
        basicBlocks.addAll(region1.getBasicBlocks());
        basicBlocks.addAll(region2.getBasicBlocks());
        basicBlocks.add(region2.getExit());
        if (!basicBlocks.containsAll(cfg.getSuccessorsOf(region1.getExit()))) {
            return false;
        }
        if (!basicBlocks.containsAll(cfg.getPredecessorsOf(region1.getExit()))) {
            return false;
        }
        return true;
    }

    private void findNonCanonicalRegions(Region region) {
        Set<Region> childrenBefore = new HashSet<Region>(region.getChildren());
        if (region.getChildCount() > 1) {
            boolean found = true;
            while (found) {
                found = false;

                // children ordered by dominance
                List<Region> children = new ArrayList<Region>(region.getChildren());
                Collections.sort(children, new Comparator<Region>() {
                    @Override
                    public int compare(Region region1, Region region2) {
                        return getDomInfo().dominates(region1.getEntry(), region2.getEntry()) ? 1 : -1;
                    }
                });

                for (Region child1 : children) {
                    for (Region child2 : children) {
                        if (!child1.equals(child2)) {
                            if (isNonCanonicalRegion(child1, child2)) {
                                Region newRegion = new Region(child1.getEntry(),
                                                              child2.getExit(),
                                                              ParentType.UNDEFINED);
                                LOGGER.debug("New non canonical region {}", newRegion);
                                child1.setParent(newRegion);
                                child2.setParent(newRegion);
                                newRegion.setParent(region);
                                found = true;
                                break;
                            }
                        }
                    }
                    if (found) {
                        break;
                    }
                }
            }
        }
        for (Region child : childrenBefore) {
            findNonCanonicalRegions(child);
        }
    }

    public RPST build() {
        // reset basic blocks parent
        for (BasicBlock bb : cfg.getBasicBlocks()) {
            bb.setRegion(null);
        }

        // find canonical regions
        Map<BasicBlock, BasicBlock> shortCut = new HashMap<BasicBlock, BasicBlock>();
        detectRegions(shortCut);

        // build tree of canonical regions
        Region rootRegion = new Region(cfg.getEntryBlock(), null, ParentType.ROOT);
        buildRegionTree(cfg.getEntryBlock(), rootRegion);

        // insert dummy region for each basic block
        for (BasicBlock bb : cfg.getBasicBlocks()) {
            Region oldParent = bb.getRegion();
            Region newParent = new Region(bb, bb, ParentType.BASIC_BLOCK);
            bb.setRegion(newParent);
            newParent.setParent(oldParent);
        }

        // add non canonical region to the tree
        findNonCanonicalRegions(rootRegion);

        return new RPST(cfg, rootRegion);
    }
}
