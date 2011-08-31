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

import de.hpi.bpt.graph.DirectedEdge;
import de.hpi.bpt.graph.MultiDirectedGraph;
import de.hpi.bpt.graph.abs.IDirectedEdge;
import de.hpi.bpt.graph.algo.rpst.RPST;
import de.hpi.bpt.graph.algo.rpst.RPSTNode;
import de.hpi.bpt.hypergraph.abs.Vertex;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.MutableTree;
import fr.jamgotchian.abcd.core.graph.Trees;
import java.util.HashMap;
import java.util.Map;

/**
 * Build a refined program tree structure from a region graph.
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RPSTBuilder {

    private final RegionGraph rg;

    public RPSTBuilder(RegionGraph rg) {
        this.rg = rg;
    }

    public MutableTree<RegionGraph, Object> build() {
        // convert abcd graph to jbpt graph
        MultiDirectedGraph rg2 = convertRG(rg);

        // build RPST with jbpt library
        RPST<DirectedEdge, Vertex> rpst = new RPST<DirectedEdge, Vertex>(rg2);

        // convert jbpt RPST to abcd RPST
        return convertRPST(rpst);
    }

    private static MultiDirectedGraph convertRG(RegionGraph rg) {
        MultiDirectedGraph rg2 = new MultiDirectedGraph();
        Map<Region, Vertex> r2v = new HashMap<Region, Vertex>();
        for (Region r : rg.getRegions()) {
            Vertex v = new Vertex();
            v.setTag(r);
            rg2.addVertex(v);
            r2v.put(r, v);
        }
        for (Edge e : rg.getEdges()) {
            Region source = rg.getSource(e);
            Region target = rg.getTarget(e);
            rg2.addEdge(r2v.get(source), r2v.get(target));
        }
        return rg2;
    }

    private MutableTree<RegionGraph, Object> convertRPST(RPST<DirectedEdge, Vertex> rpst) {
        RegionGraph root2 = convertFragment(rpst.getRoot());
        MutableTree<RegionGraph, Object> rpst2 = Trees.newTree(root2);
        for (RPSTNode<DirectedEdge, Vertex> child : rpst.getChildren(rpst.getRoot())) {
            convertRPSTNode(rpst, child, rpst2, root2);
        }
        return rpst2;
    }

    private void convertRPSTNode(RPST<DirectedEdge, Vertex> rpst,
                                 RPSTNode<DirectedEdge, Vertex> node,
                                 MutableTree<RegionGraph, Object> rpst2,
                                 RegionGraph parent) {
        RegionGraph node2 = convertFragment(node);
        rpst2.addNode(parent, node2, new Object());
        for (RPSTNode<DirectedEdge, Vertex> child : rpst.getChildren(node)) {
            convertRPSTNode(rpst, child, rpst2, node2);
        }
    }

    private RegionGraph convertFragment(RPSTNode<DirectedEdge, Vertex> node) {
        RegionGraph fragment = new RegionGraph();
        for (Vertex v : node.getFragment().getVertices()) {
            Region r = (Region) v.getTag();
            fragment.addRegion(r);
            if (v.equals(node.getEntry())) {
                fragment.setEntry(r);
            }
            if (v.equals(node.getExit())) {
                fragment.setExit(r);
            }
        }
        for (IDirectedEdge<Vertex> e : node.getFragmentEdges()) {
            Region source = (Region) e.getSource().getTag();
            Region target = (Region) e.getTarget().getTag();
            fragment.addEdge(source, target, rg.getEdge(source, target));
        }
        return fragment;
    }
}
