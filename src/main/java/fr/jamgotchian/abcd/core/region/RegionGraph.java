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
import fr.jamgotchian.abcd.core.controlflow.EdgeAttribute;
import fr.jamgotchian.abcd.core.controlflow.EdgeAttributeFactory;
import fr.jamgotchian.abcd.core.graph.AttributeFactory;
import fr.jamgotchian.abcd.core.graph.GraphvizDigraph;
import fr.jamgotchian.abcd.core.graph.DirectedGraphs;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import fr.jamgotchian.abcd.core.graph.Tree;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Represent a single entry, single exit region.
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RegionGraph implements GraphvizDigraph<Region, Edge> {

    private static final Logger logger = Logger.getLogger(RegionGraph.class.getName());

    private final String name;

    private final MutableDirectedGraph<Region, Edge> graph;

    private Region entry;

    private Region exit;

    public RegionGraph(String name) {
        this.name = name;
        graph = DirectedGraphs.newDirectedGraph();
    }

    public RegionGraph(String name, Region entry, Region exit) {
        this(name);
        this.entry = entry;
        this.exit = exit;
    }

    public RegionGraph(ControlFlowGraph cfg) {
        this(cfg.getName());
        Map<BasicBlock, Region> bb2r = new HashMap<BasicBlock, Region>();
        for (BasicBlock bb : cfg.getBasicBlocks()) {
            Region r = new BasicBlockRegion(bb);
            bb2r.put(bb, r);
            graph.addVertex(r);
        }
        for (Edge edge : cfg.getEdges()) {
            if (edge.hasAttribute(EdgeAttribute.FAKE_EDGE)) {
                continue;
            }
            BasicBlock source = cfg.getEdgeSource(edge);
            BasicBlock target = cfg.getEdgeTarget(edge);
            Region sourceRegion = bb2r.get(source);
            Region targetRegion = bb2r.get(target);
            graph.addEdge(sourceRegion, targetRegion, edge);
        }
        entry = bb2r.get(cfg.getEntryBlock());
        exit = bb2r.get(cfg.getExitBlock());

        updateAttributes();
    }

    public void updateAttributes() {
        for (Region region : getRegions()) {
            region.removeAttribute(RegionAttribute.RETURN_REGION);
            region.removeAttribute(RegionAttribute.LOOP_EXIT_REGION);
            int loopExitEdgeCount = 0;
            int returnEdgeCount = 0;
            Collection<Edge> incomingEdges = getIncomingEdgesOf(region);
            for (Edge edge : incomingEdges) {
                if (edge.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                    loopExitEdgeCount++;
                } else {
                    Region region2 = getEdgeSource(edge);
                    if (getSuccessorCountOf(region2) == 1
                            && getPredecessorCountOf(region2) == 1) {
                        Edge edge2 = getFirstIncomingEdgeOf(region2);
                        if (edge2.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                            loopExitEdgeCount++;
                        }
                    }
                }
                if (edge.hasAttribute(EdgeAttribute.RETURN_EDGE)) {
                    returnEdgeCount++;
                }
            }
            if (loopExitEdgeCount > 0 && loopExitEdgeCount == incomingEdges.size()) {
                region.addAttribute(RegionAttribute.LOOP_EXIT_REGION);
                logger.log(Level.FINE, "Found loop exit region {0}", region);
            }
            if (returnEdgeCount == incomingEdges.size()) {
                region.addAttribute(RegionAttribute.RETURN_REGION);
            }
        }
    }

    public RegionGraph(RegionGraph other) {
        name = other.name;
        graph = other.graph.clone();
        entry = other.entry;
        exit = other.exit;
    }

    public String getName() {
        return name;
    }

    public void reduceRegion(Region structuredRegion) {
        logger.log(Level.FINER, "Find {0} region : {1} => {2}",
                new Object[] {structuredRegion.getTypeName(),
                              structuredRegion.getChildRegions().toString(),
                              structuredRegion});

        structuredRegion.reduce(this);

        if (entry.equals(structuredRegion.getEntryRegion())) {
            logger.log(Level.FINEST, "New entry region : {0}", structuredRegion);
            entry = structuredRegion;
        }
        if (exit.equals(structuredRegion.getExitRegion())) {
            logger.log(Level.FINEST, "New exit region : {0}", structuredRegion);
            exit = structuredRegion;
        }
    }

    public MutableDirectedGraph<Region, Edge> getGraph() {
        return graph;
    }

    public Region getEntry() {
        return entry;
    }

    public void setEntry(Region entry) {
        this.entry = entry;
    }

    public Region getExit() {
        return exit;
    }

    public void setExit(Region exit) {
        this.exit = exit;
    }

    public void addRegion(Region region) {
        graph.addVertex(region);
    }

    public void addEdge(Region source, Region target, Edge edge) {
        graph.addEdge(source, target, edge);
    }

    public Set<Region> getRegions() {
        return graph.getVertices();
    }

    public Collection<Edge> getEdges() {
        return graph.getEdges();
    }

    public Region getEdgeSource(Edge e) {
        return graph.getEdgeSource(e);
    }

    public Region getEdgeTarget(Edge e) {
        return graph.getEdgeTarget(e);
    }

    public boolean containsRegion(Region r) {
        return graph.containsVertex(r);
    }

    public void removeRegion(Region r) {
        graph.removeVertex(r);
    }

    public void removeEdge(Edge e) {
        graph.removeEdge(e);
    }

    public Edge getEdge(Region source, Region target) {
        return graph.getEdge(source, target);
    }

    private static boolean keepEdge(Edge edge, boolean exceptional) {
        return !edge.hasAttribute(EdgeAttribute.FAKE_EDGE)
                && edge.isExceptional() == exceptional;
    }

    public Collection<Edge> getOutgoingEdgesOf(Region region, boolean exceptional) {
        List<Edge> outgoingEdges = new ArrayList<Edge>();
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                outgoingEdges.add(edge);
            }
        }
        return outgoingEdges;
    }

    public Collection<Edge> getIncomingEdgesOf(Region region, boolean exceptional) {
        List<Edge> incomingEdges = new ArrayList<Edge>();
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                incomingEdges.add(edge);
            }
        }
        return incomingEdges;
    }

    public Collection<Region> getSuccessorsOf(Region region, boolean exceptional) {
        List<Region> successors = new ArrayList<Region>();
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                successors.add(graph.getEdgeTarget(edge));
            }
        }
        return successors;
    }

    public Collection<Region> getPredecessorsOf(Region region, boolean exceptional) {
        List<Region> predecessors = new ArrayList<Region>();
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                predecessors.add(graph.getEdgeSource(edge));
            }
        }
        return predecessors;
    }

    public Edge getFirstOutgoingEdgeOf(Region region, boolean exceptional) {
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                return edge;
            }
        }
        return null;
    }

    public Edge getFirstIncomingEdgeOf(Region region, boolean exceptional) {
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                return edge;
            }
        }
        return null;
    }

    public Region getFirstSuccessorOf(Region region, boolean exceptional) {
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                return graph.getEdgeTarget(edge);
            }
        }
        return null;
    }

    public Region getFirstPredecessorOf(Region region, boolean exceptional) {
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                return graph.getEdgeSource(edge);
            }
        }
        return null;
    }

    public int getSuccessorCountOf(Region region, boolean exceptional) {
        int count = 0;
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                count++;
            }
        }
        return count;
    }

    public int getPredecessorCountOf(Region region, boolean exceptional) {
        int count = 0;
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if (keepEdge(edge, exceptional)) {
                count++;
            }
        }
        return count;
    }

    public Collection<Edge> getIncomingEdgesOf(Region r) {
        return graph.getIncomingEdgesOf(r);
    }

    public Collection<Edge> getOutgoingEdgesOf(Region r) {
        return graph.getOutgoingEdgesOf(r);
    }

    public Edge getFirstIncomingEdgeOf(Region r) {
        return graph.getFirstIncomingEdgeOf(r);
    }

    public Edge getFirstOutgoingEdgesOf(Region r) {
        return graph.getFirstOutgoingEdgeOf(r);
    }

    public Region getFirstPredecessorOf(Region r) {
        return graph.getFirstPredecessorOf(r);
    }

    public Region getFirstSuccessorOf(Region r) {
        return graph.getFirstSuccessorOf(r);
    }

    public int getPredecessorCountOf(Region r) {
        return graph.getPredecessorCountOf(r);
    }

    public int getSuccessorCountOf(Region r) {
        return graph.getSuccessorCountOf(r);
    }

    public Tree<Region, Edge> getReversePostOrderDFST(Region root) {
        return graph.getReversePostOrderDFST(root, false);
    }

    public String getClusterID() {
        return graph.getClusterID();
    }

    public void export(Writer writer, String name,
                       AttributeFactory<Region> vertexAttrFactory,
                       AttributeFactory<Edge> edgeAttrFactory,
                       boolean isSubgraph) throws IOException {
        boolean useConstraints = graph.getEdgeCount() > 1;
        graph.export(writer, name, new RegionAttributeFactory(),
                                   new EdgeAttributeFactory(useConstraints),
                                   isSubgraph);
    }

    public void export(Writer writer) throws IOException {
        boolean useConstraints = graph.getEdgeCount() > 1;
        graph.export(writer, name, new RegionAttributeFactory(),
                                   new EdgeAttributeFactory(useConstraints));
    }

    public String toString(Collection<Edge> edges) {
        return graph.toString(edges);
    }

    public String toString(Edge edge) {
        return graph.toString(edge);
    }

    @Override
    public RegionGraph clone() {
        return new RegionGraph(this);
    }

    @Override
    public String toString() {
        return "(" + entry + ", " + exit + ")";
    }
}
