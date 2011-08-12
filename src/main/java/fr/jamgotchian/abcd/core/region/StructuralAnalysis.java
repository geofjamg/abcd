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
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.DominatorInfo;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeAttribute;
import fr.jamgotchian.abcd.core.controlflow.EdgeFactoryImpl;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.graph.DirectedGraphs;
import fr.jamgotchian.abcd.core.graph.EdgeFactory;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import fr.jamgotchian.abcd.core.graph.MutableTree;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.graph.Trees;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class StructuralAnalysis {

    private static final Logger logger = Logger.getLogger(StructuralAnalysis.class.getName());

    private static final EdgeFactory<Edge> EDGE_FACTORY = new EdgeFactoryImpl();

    private static final List<RegionRecognizer> RECOGNIZERS
            = Collections.unmodifiableList(Arrays.asList(/* first, acyclic regions */
                                                         new BlockRecognizer(),
                                                         new IfThenElseRecognizer(),
                                                         new SwitchCaseRecognizer(),
                                                         /* then, cyclic regions */
                                                         new LoopRecognizer(),
                                                         /* try catch regions */
                                                         new InlinedFinallyBreakRecognizer(),
                                                         new TryCatchFinallyRecognizer()));

    private final ControlFlowGraph CFG;

    private MutableDirectedGraph<Region, Edge> regionGraph;

    private static class CheckResult {

        private final boolean failed;

        private final Region entryRegion;

        private CheckResult(boolean failed, Region entryRegion) {
            this.failed = failed;
            this.entryRegion = entryRegion;
        }

        private boolean hasFailed() {
            return failed;
        }

        private Region getEntryRegion() {
            return entryRegion;
        }
    }

    public StructuralAnalysis(ControlFlowGraph CFG) {
        this.CFG = CFG;
    }

    public DirectedGraph<Region, Edge> getRegionGraph() {
        return DirectedGraphs.unmodifiableDirectedGraph(regionGraph);
    }

    private static void buildControlTree(Region parentRegion, MutableTree<Region, Edge> controlTree) {
        for (Region childRegion : parentRegion.getChildRegions()) {
           controlTree.addNode(parentRegion, childRegion, new EdgeImpl());
           buildControlTree(childRegion, controlTree);
        }
    }

    private Region reduceRegion(Region structuredRegion, Region entryRegion) {
        logger.log(Level.FINER, "  Find {0} region : {1} => {2}",
                new Object[] {structuredRegion.getTypeName(),
                    structuredRegion.getChildRegions().toString(), structuredRegion});
//        logger.log(Level.FINER, "  ENTRY " + entryRegion);

//        logger.log(Level.FINEST, "  Internal edges : {0}",
//                regionGraph.toString(structuredRegion.getChildEdges()));

        structuredRegion.reduce(regionGraph);

        if (structuredRegion.getEntryRegion().equals(entryRegion)) {
            logger.log(Level.FINEST, "    New entry region : {0}", structuredRegion);
            return structuredRegion;
        } else {
            return entryRegion;
        }
    }

    private CheckResult checkRegions(List<Region> regions, Region entryRegion) {
        boolean failed = true;
        for (Region region : regions) {
            Region structuredRegion = null;
            for (RegionRecognizer recognizer : RECOGNIZERS) {
//                logger.log(Level.FINEST, "Check for region with {0} at {1}",
//                        new Object[] {recognizer.getClass().getSimpleName(), region});
                structuredRegion = recognizer.recognize(regionGraph, region);
                if (structuredRegion != null) {
                    break;
                }
            }
            if (structuredRegion != null) {
                entryRegion = reduceRegion(structuredRegion, entryRegion);
                failed = false;
                break;
            }
        }
        return new CheckResult(failed, entryRegion);
    }

    private void reduceRegionGraph() {
        Map<BasicBlock, Region> block2region = new HashMap<BasicBlock, Region>();
        regionGraph = DirectedGraphs.newDirectedGraph();
        for (BasicBlock block : CFG.getBasicBlocks()) {
            Region region = new BasicBlockRegion(block);
            block2region.put(block, region);
            regionGraph.addVertex(region);
        }
        for (Edge edge : CFG.getEdges()) {
            BasicBlock source = CFG.getEdgeSource(edge);
            BasicBlock target = CFG.getEdgeTarget(edge);
            regionGraph.addEdge(block2region.get(source), block2region.get(target), edge);
        }
        Region entryRegion = block2region.get(CFG.getEntryBlock());
        Region exitRegion = block2region.get(CFG.getExitBlock());

        reduceRegionSubGraph(entryRegion, exitRegion);
    }

    private List<Region> getSubgraphRegions(Region entryRegion, Region exitRegion, boolean invert) {
        Region region1 = invert ? exitRegion : entryRegion;
        Region region2 = invert ? entryRegion : exitRegion;
        Tree<Region, Edge> DFST
            = regionGraph.getReversePostOrderDFST(region1, Sets.newHashSet(region2), invert);
        List<Region> regions = new ArrayList<Region>(DFST.getNodes().size()+1);
        regions.addAll(DFST.getNodes());
        regions.add(region2);
        return regions;
    }

    private boolean reduceRegionSubGraph(Region entryRegion, Region exitRegion) {
        logger.log(Level.FINER, "Try to reduce subgraph ({0},{1})",
                new Object[] {entryRegion, exitRegion});

        Collection<Edge> incomingEdgesOfExit = regionGraph.getIncomingEdgesOf(exitRegion);
        logger.log(Level.FINEST, "  Cut incoming edges of {0} : {1}",
                new Object[] {exitRegion, regionGraph.toString(incomingEdgesOfExit)});

        for (Edge incomingEdge : new ArrayList<Edge>(incomingEdgesOfExit)) {
            if (incomingEdge.hasAttribute(EdgeAttribute.FAKE_EDGE)) {
                continue;
            }
            Region sourceRegion = regionGraph.getEdgeSource(incomingEdge);
            if (Regions.getSuccessorCountOf(regionGraph, sourceRegion, false) == 1) {
                incomingEdge.addAttribute(EdgeAttribute.FAKE_EDGE);
                sourceRegion.setBreak(true);
            } else {
                regionGraph.removeEdge(incomingEdge);
                Region emptyRegion = new EmptyRegion();
                regionGraph.addVertex(emptyRegion);
                regionGraph.addEdge(sourceRegion, emptyRegion, incomingEdge);
                Edge fakeEdge = EDGE_FACTORY.createEdge();
                fakeEdge.addAttribute(EdgeAttribute.FAKE_EDGE);
                regionGraph.addEdge(emptyRegion, exitRegion, fakeEdge);
                Regions.copyHandlers(regionGraph, sourceRegion, emptyRegion);
                emptyRegion.setBreak(true);
            }
        }

        boolean reduced = false;
        boolean failed = false;
        while (!reduced && !failed) {
            List<Region> regions = getSubgraphRegions(entryRegion, exitRegion, false);

            if (regions.size() == 2) {
                reduced = true;
            } else {
                // check regions in reverse order
                Collections.reverse(regions);

                CheckResult result = checkRegions(regions, entryRegion);
                failed = result.hasFailed();
                entryRegion = result.getEntryRegion();

                if (failed) {
                    logger.log(Level.FINEST, "Fail to reduce regions in subgraph ({0},{1})",
                            new Object[] {entryRegion, exitRegion});

                    List<Region> regions2 = getSubgraphRegions(entryRegion, exitRegion, true);

                    for (Region joinRegion : regions2) {
                        boolean loopExitFound = false;
                        for (Edge edge : regionGraph.getIncomingEdgesOf(joinRegion)) {
                            if (edge.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                                loopExitFound = true;
                                break;
                            }
                        }
                        if (loopExitFound) {
                            DominatorInfo<Region, Edge> dominatorInfo
                                    = DominatorInfo.create(regionGraph, entryRegion, EDGE_FACTORY);
                            Region forkRegion = dominatorInfo.getDominatorsTree().getParent(joinRegion);

                            if (!forkRegion.equals(entryRegion)
                                    && !joinRegion.equals(exitRegion)) {
                                failed = reduceRegionSubGraph(forkRegion, joinRegion);
                            }
                            break;
                        }
                    }

//                    for (Region joinRegion : regions) {
//                        if (regionGraph.getPredecessorCountOf(joinRegion) > 1) {
//                            DominatorInfo<Region, Edge> dominatorInfo
//                                    = DominatorInfo.create(regionGraph, entryRegion, EDGE_FACTORY);
//                            Region forkRegion = dominatorInfo.getDominatorsTree().getParent(joinRegion);
//
//                            if (!forkRegion.equals(entryRegion)
//                                    && !joinRegion.equals(exitRegion)) {
//                                failed = reduceRegionSubGraph(forkRegion, joinRegion);
//                            }
//                            break;
//                        }
//                    }
                }
            }
        }

        if (reduced) {
            Edge exitEdge = regionGraph.getEdge(entryRegion, exitRegion);
            if (exitEdge == null) {
                exitEdge = EDGE_FACTORY.createEdge();
                regionGraph.addEdge(entryRegion, exitRegion, exitEdge);
            } else {
                exitEdge.removeAttribute(EdgeAttribute.FAKE_EDGE);
            }
            Region labeledRegion = new LabeledRegion(entryRegion);
            entryRegion = reduceRegion(labeledRegion, entryRegion);
            BlockRegion blockRegion = new BlockRegion(Collections.singletonList(exitEdge),
                                                      Arrays.asList(labeledRegion, exitRegion));
            entryRegion = reduceRegion(blockRegion, entryRegion);

            logger.log(Level.FINEST, "Subgraph ({0},{1}) reduced",
                    new Object[] {entryRegion, exitRegion});

            return false;
        } else {
            logger.log(Level.FINEST, "Fail to recognize subgraph in subgraph ({0},{1})",
                    new Object[] {entryRegion, exitRegion});

            return true;
        }
    }

    public DirectedGraph<Region, Edge> analyse() {
        // reduce the region graph
        reduceRegionGraph();

        Set<Region> rootRegions = regionGraph.getVertices();
        if (rootRegions.size() == 1) {
            Region rootRegion = rootRegions.iterator().next();
            MutableTree<Region, Edge> controlTree = Trees.newTree(rootRegion);
            buildControlTree(rootRegion, controlTree);
            logger.log(Level.FINEST, "Control tree :\n{0}", Trees.toString(controlTree));
        }
        return regionGraph;
    }
}
