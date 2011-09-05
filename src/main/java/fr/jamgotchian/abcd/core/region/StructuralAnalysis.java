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

import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.EdgeAttribute;
import fr.jamgotchian.abcd.core.controlflow.EdgeFactoryImpl;
import fr.jamgotchian.abcd.core.controlflow.EdgeImpl;
import fr.jamgotchian.abcd.core.graph.EdgeFactory;
import fr.jamgotchian.abcd.core.graph.MutableTree;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.graph.Trees;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
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

    private final ControlFlowGraph cfg;

    public StructuralAnalysis(ControlFlowGraph cfg) {
        this.cfg = cfg;
    }

    private boolean checkRegions(RegionGraph graph) {
        boolean failed = true;
        Tree<Region, Edge>  dfst = graph.getReversePostOrderDFST(graph.getEntry());
        List<Region> regions = new ArrayList<Region>(dfst.getNodes());
        Collections.reverse(regions);
        for (Region region : regions) {
            Region structuredRegion = null;
            for (RegionRecognizer recognizer : RECOGNIZERS) {
//                logger.log(Level.FINEST, "Check for region with {0} at {1}",
//                        new Object[] {recognizer.getClass().getSimpleName(), region});
                structuredRegion = recognizer.recognize(graph, region);
                if (structuredRegion != null) {
                    break;
                }
            }
            if (structuredRegion != null) {
                graph.reduceRegion(structuredRegion);
                failed = false;
                break;
            }
        }
        return failed;
    }

    private void cutEdge(RegionGraph graph, Edge edge) {
        if (edge.hasAttribute(EdgeAttribute.FAKE_EDGE)) {
            return;
        }
        logger.log(Level.FINEST, "  Cut incoming edge {0}", graph.toString(edge));
        Region sourceRegion = graph.getEdgeSource(edge);
//        Region targetRegion = graph.getEdgeSource(edge);
        if (graph.getSuccessorCountOf(sourceRegion, false) == 1) {
            graph.removeEdge(edge);
//            edge.addAttribute(EdgeAttribute.FAKE_EDGE);
            sourceRegion.setBreak(true);
        } else {
            graph.removeEdge(edge);
            Region emptyRegion = new EmptyRegion();
            graph.addRegion(emptyRegion);
            graph.addEdge(sourceRegion, emptyRegion, edge);
//            Edge fakeEdge = EDGE_FACTORY.createEdge();
//            fakeEdge.addAttribute(EdgeAttribute.FAKE_EDGE);
//            graph.addEdge(emptyRegion, targetRegion, fakeEdge);
            Regions.copyHandlers(graph, sourceRegion, emptyRegion);
            emptyRegion.setBreak(true);
        }
    }

    private boolean reduceRegionGraph(RegionGraph graph) {
        logger.log(Level.FINER, "Try to reduce region graph {0}", graph);

        boolean failed = false;
        while (!failed) {
            failed = checkRegions(graph);
//
//            if (failed && graph.getRegions().size() > 1) {
//                logger.log(Level.FINER, "Failed to recognize regions in {0}", graph);
//
//                RegionGraph breakSubgraph = null;
//
//                Tree<RegionGraph, Object> rpst = new RPSTBuilder(graph).build();
//                List<RegionGraph> fragments = new ArrayList<RegionGraph>(rpst.getNodes());
//                Collections.reverse(fragments);
//                TOTO : for (RegionGraph fragment : fragments) {
//                    if (fragment.getPredecessorCountOf(fragment.getExit()) > 1) {
//                        for (Edge edge : fragment.getIncomingEdgesOf(fragment.getExit())) {
//                            if (edge.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
//                                logger.log(Level.FINER, "Found break region ({0},{1})",
//                                        new Object[] {fragment.getEntry(), fragment.getExit()});
//
//                                // create break subgraph and remove it from parent
//                                // graph
//                                breakSubgraph = new RegionGraph();
//                                Region newEntry = new EmptyRegion();
//                                Region newExit = new EmptyRegion();
//                                breakSubgraph.addRegion(newEntry);
//                                breakSubgraph.addRegion(newExit);
//                                breakSubgraph.setEntry(newEntry);
//                                breakSubgraph.setExit(newExit);
//                                for (Region r : fragment.getRegions()) {
//                                    if (!r.equals(fragment.getEntry()) &&
//                                            !r.equals(fragment.getExit())) {
//                                        breakSubgraph.addRegion(r);
//                                    }
//                                }
//                                for (Edge e : fragment.getEdges()) {
//                                    Region s = fragment.getEdgeSource(e);
//                                    Region t = fragment.getEdgeTarget(e);
//                                    if (s.equals(fragment.getEntry())) {
//                                        s = newEntry;
//                                    } else if (s.equals(fragment.getExit())) {
//                                        s = newExit;
//                                    }
//                                    if (t.equals(fragment.getEntry())) {
//                                        t = newEntry;
//                                    } else if (t.equals(fragment.getExit())) {
//                                        t = newExit;
//                                    }
//                                    breakSubgraph.addEdge(s, t, e);
//                                }
//                                for (Edge e : breakSubgraph.getEdges()) {
//                                    graph.removeEdge(e);
//                                }
//                                for (Region r : breakSubgraph.getRegions()) {
//                                    if (!r.equals(newEntry) && !r.equals(newExit)) {
//                                        graph.removeRegion(r);
//                                    }
//                                }
//                                for (Edge e : new ArrayList<Edge>(breakSubgraph.getIncomingEdgesOf(newExit))) {
//                                    if (e.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
//                                        breakSubgraph.removeEdge(e);
//                                    }
//                                }
//                                if (breakSubgraph.getPredecessorCountOf(newExit) == 0) {
//                                    breakSubgraph.removeRegion(newExit);
//                                }
//                                logger.info(DirectedGraphs.toString(breakSubgraph.getGraph(), breakSubgraph.getEntry()));
//
//                                failed = reduceRegionGraph(breakSubgraph);
//                                if (!failed) {
//                                    LabeledRegion labRegion = new LabeledRegion(breakSubgraph.getRegions().iterator().next());
//                                    graph.addRegion(labRegion);
//                                    graph.addEdge(fragment.getEntry(), labRegion, new EdgeImpl());
//                                    graph.addEdge(labRegion, fragment.getExit(), new EdgeImpl());
//                                }
//                                break TOTO;
//                            }
//                        }
//                    }
//                }
//            }
        }

        failed = graph.getRegions().size() > 1;

        return failed;
    }

    private static void buildControlTree(Region parentRegion, MutableTree<Region, Edge> controlTree) {
        for (Region childRegion : parentRegion.getChildRegions()) {
           controlTree.addNode(parentRegion, childRegion, new EdgeImpl());
           buildControlTree(childRegion, controlTree);
        }
    }

    private void printControlTree(RegionGraph graph) {
        if (graph.getRegions().size() == 1) {
            Region rootRegion = graph.getRegions().iterator().next();
            MutableTree<Region, Edge> controlTree = Trees.newTree(rootRegion);
            buildControlTree(rootRegion, controlTree);
            logger.log(Level.FINEST, "Control tree :\n{0}", Trees.toString(controlTree));
        }
    }

    public RegionGraph analyse() {
        RegionGraph graph = new RegionGraph(cfg);

        boolean failed = reduceRegionGraph(graph);

//        if (failed) {
//            // try to cut return edges
//            Region exitRegion = graph.getExit();
//            for (Edge edge : new ArrayList<Edge>(graph.getIncomingEdgesOf(exitRegion))) {
//                if (edge.hasAttribute(EdgeAttribute.RETURN_EDGE)) {
//                    cutEdge(graph, edge);
//                }
//            }
//            if (graph.getPredecessorCountOf(exitRegion) == 0) {
//                graph.removeRegion(exitRegion);
//
//                // reduce the region graph
//                failed = reduceRegionGraph(graph);
//                if (!failed) {
//                    Region rootRegion = graph.getRegions().iterator().next();
//                    Edge exitEdge = new EdgeImpl();
//                    graph.addRegion(exitRegion);
//                    graph.addEdge(rootRegion, exitRegion, exitEdge);
//                    BlockRegion blockRegion = new BlockRegion(Arrays.asList(exitEdge),
//                                                              Arrays.asList(rootRegion, exitRegion));
//                    blockRegion.reduce(graph);
//
//                    // print control tree
//                    printControlTree(graph);
//                }
//            } else {
//                failed = reduceRegionGraph(graph);
//            }
//        }

        return graph;
    }
}
