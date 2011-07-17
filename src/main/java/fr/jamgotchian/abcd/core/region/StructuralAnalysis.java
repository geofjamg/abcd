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

import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.graph.DirectedGraphs;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import fr.jamgotchian.abcd.core.graph.MutableTree;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.graph.Trees;
import java.util.ArrayList;
import java.util.Arrays;
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

    private final List<RegionRecognizer> recognizers;
    private final List<RegionRecognizer> recognizers2;

    private final ControlFlowGraph graph;

    private MutableDirectedGraph<Region, Edge> regionGraph;
    private Region entryRegion;
    private Region exitRegion;

    public StructuralAnalysis(ControlFlowGraph graph) {
        this.graph = graph;
        recognizers = Collections.unmodifiableList(Arrays.asList(/* first, acyclic regions */
                                                                 new BlockRecognizer(true),
                                                                 new IfThenElseRecognizer(),
                                                                 new SwitchCaseRecognizer(),
                                                                 /* then, cyclic regions */
                                                                 new LoopRecognizer(),
                                                                 /* try catch regions */
                                                                 new TryCatchRecognizer()));

        recognizers2 = Collections.<RegionRecognizer>unmodifiableList(Arrays.asList(new BlockRecognizer(false)));
    }

    public DirectedGraph<Region, Edge> getRegionGraph() {
        return DirectedGraphs.unmodifiableDirectedGraph(regionGraph);
    }

    private void collapseRegion(Region structuredRegion) {
        logger.log(Level.FINER, "Find {0} region : {1} => {2}",
                new Object[] {structuredRegion.getTypeName(),
                    structuredRegion.getChildRegions().toString(), structuredRegion});

        logger.log(Level.FINEST, "  Internal edges : {0}",
                regionGraph.toString(structuredRegion.getChildEdges()));

        structuredRegion.collapse(regionGraph);

        switch (structuredRegion.getType()) {
            case IF_THEN_BREAK:
                logger.log(Level.FINER, "  Break target : {0}",
                        ((IfThenBreakRegion) structuredRegion).getBreakTargetRegion());
                break;
            case LOOP: {
                logger.log(Level.FINER, "  Loop type : {0}", ((LoopRegion) structuredRegion).getLoopType());
                logger.log(Level.FINER, "  Natural exit : {0}",
                        regionGraph.getFirstSuccessorOf(structuredRegion));
                break;
            }
        }

        if (structuredRegion.getEntryRegion().equals(entryRegion)) {
            logger.log(Level.FINEST, "  New entry region : {0}", structuredRegion);
            entryRegion = structuredRegion;
        }
    }

    private static void buildControlTree(Region parentRegion, MutableTree<Region, Edge> controlTree) {
        for (Region childRegion : parentRegion.getChildRegions()) {
           controlTree.addNode(parentRegion, childRegion, new EdgeImpl());
           buildControlTree(childRegion, controlTree);
        }
    }

    private boolean checkRegions(List<Region> nodes, List<RegionRecognizer> recognizers) {
        boolean failed = true;
        for (Region region : nodes) {
            Region structuredRegion = null;
            for (RegionRecognizer recognizer : recognizers) {
                logger.log(Level.FINEST, "Check for region with {0} at {1}",
                        new Object[] {recognizer.getClass().getSimpleName(), region});
                structuredRegion = recognizer.recognize(regionGraph, region);
                if (structuredRegion != null) {
                    break;
                }
            }
            if (structuredRegion != null) {
                collapseRegion(structuredRegion);
                failed = false;
                break;
            }
        }
        return failed;
    }

    private void buildRegionGraph() {
        Map<BasicBlock, Region> block2region = new HashMap<BasicBlock, Region>();
        regionGraph = DirectedGraphs.newDirectedGraph();
        for (BasicBlock block : graph.getBasicBlocks()) {
            Region region = new BasicBlockRegion(block);
            block2region.put(block, region);
            regionGraph.addVertex(region);
        }
        for (Edge edge : graph.getEdges()) {
            BasicBlock source = graph.getEdgeSource(edge);
            BasicBlock target = graph.getEdgeTarget(edge);
            regionGraph.addEdge(block2region.get(source), block2region.get(target), edge);
        }
        entryRegion = block2region.get(graph.getEntryBlock());
        exitRegion = block2region.get(graph.getExitBlock());
    }

    private void reduceRegionGraph() {
        boolean failed = false;
        while (regionGraph.getVertexCount() > 1 && !failed) {
            Tree<Region, Edge> dfst = regionGraph.getReversePostOrderDFST(entryRegion, false);
            List<Region> reverseNodes = new ArrayList<Region>(dfst.getNodes());
            Collections.reverse(reverseNodes);
            failed = checkRegions(reverseNodes, recognizers);
            if (failed) {
                failed = checkRegions(reverseNodes, recognizers2);
            }
//            logger.log(Level.FINEST, "Region graph :\n{0}",
//                    DirectedGraphs.toString(regionGraph, entryRegion));
        }
    }

    public DirectedGraph<Region, Edge> analyse() {
        // build initial region graph
        buildRegionGraph();

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
