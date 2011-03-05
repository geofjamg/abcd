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

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import fr.jamgotchian.abcd.core.graph.DirectedGraphs;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import fr.jamgotchian.abcd.core.graph.MutableTree;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.graph.Trees;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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

    private static final List<RegionRecognizer> RECOGNIZERS;

    static {
        logger.setLevel(Level.FINER);

        RECOGNIZERS = Collections.unmodifiableList(Arrays.asList(/* first, acyclic regions */
                                                                 new BlockRecognizer(),
                                                                 new LogicalRecognizer(),
                                                                 new IfThenElseRecognizer(),
                                                                 new SwitchCaseRecognizer(),
                                                                 /* then, cyclic regions */
                                                                 new LoopRecognizer(),
                                                                 /* try catch regions */
                                                                 new TryCatchRecognizer()));
    }

    private final ControlFlowGraph graph;

    private MutableDirectedGraph<Region, Edge> regionGraph;
    private Region entryRegion;

    public StructuralAnalysis(ControlFlowGraph graph) {
        this.graph = graph;
    }

    private void collapseRegion(Region structuredRegion) {
        Collection<Region> internalRegions = structuredRegion.getInternalRegions();

        logger.log(Level.FINER, "Find {0} region : {1} => {2}",
                new Object[] {structuredRegion.getTypeName(),
                    internalRegions.toString(), structuredRegion});

        Collection<Edge> internalEdges = structuredRegion.getInternalEdges();

        logger.log(Level.FINEST, "  Internal edges : {0}", regionGraph.toString(internalEdges));

        Set<Region> externalPredecessors = new HashSet<Region>();
        Multimap<Region, Edge> externalSuccessors = HashMultimap.create();
        Set<Edge> externalIncomingEdges = new HashSet<Edge>();
        Set<Edge> externalOutgoingEdges = new HashSet<Edge>();
        Set<Edge> externalLoopEdges = new HashSet<Edge>();

        for (Region r : internalRegions) {
            for (Edge e : new HashSet<Edge>(regionGraph.getOutgoingEdgesOf(r))) {
                if (!internalEdges.contains(e)) {
                    Region target = regionGraph.getEdgeTarget(e);
                    if (internalRegions.contains(target)) {
                        externalLoopEdges.add(e);
                    } else {
                        externalOutgoingEdges.add(e);
                    }
                }
            }
            for (Edge e : new HashSet<Edge>(regionGraph.getIncomingEdgesOf(r))) {
                if (!internalEdges.contains(e)) {
                    Region source = regionGraph.getEdgeSource(e);
                    if (internalRegions.contains(source)) {
                        externalLoopEdges.add(e);
                    } else {
                        externalIncomingEdges.add(e);
                    }
                }
            }
        }

        for (Edge e : internalEdges) {
            Region source = regionGraph.getEdgeSource(e);
            Region target = regionGraph.getEdgeTarget(e);
            if (!internalRegions.contains(source)) {
                externalPredecessors.add(source);
            }
            if (!internalRegions.contains(target)) {
                externalSuccessors.put(target, e);
            }
        }

        logger.log(Level.FINEST, "  External incoming edges : {0}", regionGraph.toString(externalIncomingEdges));
        logger.log(Level.FINEST, "  External outgoing edges : {0}", regionGraph.toString(externalOutgoingEdges));
        logger.log(Level.FINEST, "  External predecessors : {0}", externalPredecessors);
        logger.log(Level.FINEST, "  External successors : {0}", externalSuccessors.keySet());

        regionGraph.addVertex(structuredRegion);

        for (Edge edge : externalLoopEdges) {
            regionGraph.removeEdge(edge);
            regionGraph.addEdge(structuredRegion, structuredRegion, edge);
        }
        for (Edge edge : externalIncomingEdges) {
            Region source = regionGraph.getEdgeSource(edge);
            regionGraph.removeEdge(edge);
            if (!regionGraph.containsEdge(source, structuredRegion)) {
                regionGraph.addEdge(source, structuredRegion, edge);
            }
        }
        for (Edge edge : externalOutgoingEdges) {
            Region target = regionGraph.getEdgeTarget(edge);
            regionGraph.removeEdge(edge);
            if (!regionGraph.containsEdge(structuredRegion, target)) {
                regionGraph.addEdge(structuredRegion, target, edge);
            }
        }
        for (Region region : externalPredecessors) {
            regionGraph.addEdge(region, structuredRegion, new EdgeImpl());
        }
        for (Map.Entry<Region, Collection<Edge>> entry : externalSuccessors.asMap().entrySet()) {
            Region region = entry.getKey();
            Collection<Edge> edges = entry.getValue();
            Edge syntheticEdge = structuredRegion.createSyntheticEdge(edges);
            regionGraph.addEdge(structuredRegion, region, syntheticEdge);
        }

        for (Edge edge : structuredRegion.getInternalEdges()) {
            regionGraph.removeEdge(edge);
        }
        for (Region region : structuredRegion.getInternalRegions()) {
            regionGraph.removeVertex(region);
        }

        if (structuredRegion.getEntryRegion().equals(entryRegion)) {
            logger.log(Level.FINEST, "  New entry region : {0}", structuredRegion);
            entryRegion = structuredRegion;
        }
    }

    private static void buildControlTree(Region parentRegion, MutableTree<Region, Edge> controlTree) {
        for (Region childRegion : parentRegion.getInternalRegions()) {
           controlTree.addNode(parentRegion, childRegion, new EdgeImpl());
           buildControlTree(childRegion, controlTree);
        }
    }

    public Set<Region> analyse() {
        // build initial region graph
        Map<BasicBlock, Region> block2region = new HashMap<BasicBlock, Region>();
        regionGraph = DirectedGraphs.newDirectedGraph();
        for (BasicBlock block : graph.getBasicBlocks()) {
            Region region = new LeafRegion(block);
            block2region.put(block, region);
            regionGraph.addVertex(region);
        }
        for (Edge edge : graph.getEdges()) {
            BasicBlock source = graph.getEdgeSource(edge);
            BasicBlock target = graph.getEdgeTarget(edge);
            regionGraph.addEdge(block2region.get(source), block2region.get(target), edge);
        }
        entryRegion = block2region.get(graph.getEntryBlock());

        // reduce the region graph
        boolean failed = false;
        while (regionGraph.getVertexCount() > 1 && !failed) {
            Tree<Region, Edge> dfst = regionGraph.getReversePostOrderDFST(entryRegion, false);
            failed = true;
            List<Region> reverseNodes = new ArrayList<Region>(dfst.getNodes());
            Collections.reverse(reverseNodes);
            Region structuredRegion = null;
            for (Region region : reverseNodes) {
                for (RegionRecognizer recognizer : RECOGNIZERS) {
//                    logger.log(Level.FINEST, "Check for region with {0} at {1}",
//                            new Object[] {recognizer.getClass().getSimpleName(), region});
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

//            logger.log(Level.FINEST, "Region graph :\n{0}",
//                    DirectedGraphs.toString(regionGraph, entryRegion));
        }

        Set<Region> rootRegions = regionGraph.getVertices();
        if (rootRegions.size() == 1) {
            Region rootRegion = rootRegions.iterator().next();
            MutableTree<Region, Edge> controlTree = Trees.newTree(rootRegion);
            buildControlTree(rootRegion, controlTree);
            logger.log(Level.FINEST, "Control tree :\n{0}", Trees.toString(controlTree));
        }
        return rootRegions;
    }
}
