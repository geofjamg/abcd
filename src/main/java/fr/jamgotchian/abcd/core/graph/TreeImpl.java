/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

package fr.jamgotchian.abcd.core.graph;

import fr.jamgotchian.abcd.core.common.ABCDException;
import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class TreeImpl<N, E> implements MutableTree<N, E> {

    private static class Connection<N> {

        private final N source;

        private final N target;

        private Connection(N source, N target) {
            this.source = source;
            this.target = target;
        }

        public N getSource() {
            return source;
        }

        public N getTarget() {
            return target;
        }
    }

    private static class Neighbors<N, E> {

        private N parentNode;

        private E incomingEdge;

        private final Map<N, E> children;

        private Neighbors(N parentNode, E incomingEdge, Map<N, E> children) {
            this.parentNode = parentNode;
            this.incomingEdge = incomingEdge;
            this.children = children;
        }

        private Neighbors(N parentNode, E incomingEdge) {
            this(parentNode, incomingEdge, new LinkedHashMap<N, E>());
        }

        private Neighbors() {
            this(null, null);
        }

        public Map<N, E> getChildren() {
            return children;
        }

        public N getParentNode() {
            return parentNode;
        }

        public void setParentNode(N parentNode) {
            this.parentNode = parentNode;
        }

        public E getIncomingEdge() {
            return incomingEdge;
        }

        public void setIncomingEdge(E incomingEdge) {
            this.incomingEdge = incomingEdge;
        }
    }

    private final N root;

    private final Map<E, Connection<N>> edges = new LinkedHashMap<E, Connection<N>>();

    private final Map<N, Neighbors<N,E>> nodes = new LinkedHashMap<N, Neighbors<N,E>>();

    TreeImpl(N root) {
        this.root = root;
        nodes.put(root, new Neighbors<N, E>());
    }

    public N getRoot() {
        return root;
    }

    public Set<N> getNodes() {
        return nodes.keySet();
    }

    public int getNodeCount() {
        return nodes.size();
    }

    public Set<E> getEdges() {
        return edges.keySet();
    }

    public void addNode(N parent, N node, E edge) {
        if (nodes.containsKey(node)) {
            throw new ABCDException("Node " + node + " already present");
        }
        if (edges.containsKey(edge)) {
            throw new ABCDException("Edge " + edge + " already present");
        }
        Neighbors<N,E> parentsNeighbors = nodes.get(parent);
        if (parentsNeighbors == null) {
            throw new ABCDException("Parent node " + parent + " not found");
        }
        parentsNeighbors.getChildren().put(node, edge);
        nodes.put(node, new Neighbors<N, E>(parent, edge));
        edges.put(edge, new Connection<N>(parent, node));
    }

    public boolean containsNode(N node) {
        return nodes.containsKey(node);
    }

    public N getParent(N node) {
        Neighbors<N,E> neighbors = nodes.get(node);
        if (neighbors == null) {
            throw new ABCDException("Node " + node + " not found");
        }
        return neighbors.getParentNode();
    }

    private void getAncestors(N node, Set<N> ancestors) {
        N parent = getParent(node);
        if (parent != null) {
            ancestors.add(parent);
            getAncestors(parent, ancestors);
        }
    }

    public Set<N> getAncestors(N node) {
        Set<N> ancestors = new LinkedHashSet<N>();
        getAncestors(node, ancestors);
        return ancestors;
    }

    public E getIncomingEdge(N node) {
        Neighbors<N,E> neighbors = nodes.get(node);
        if (neighbors == null) {
            throw new ABCDException("Node " + node + " not found");
        }
        return neighbors.getIncomingEdge();
    }

    public void setParent(N node, N newParent) {
        if (node.equals(root)) {
            throw new ABCDException("Can't change parent of root node");
        }
        Neighbors<N,E> neighbors = nodes.get(node);
        if (neighbors == null) {
            throw new ABCDException("Node " + node + " not found");
        }
        Neighbors<N,E> newParentNeighbors = nodes.get(newParent);
        if (newParentNeighbors == null) {
            throw new ABCDException("New parent node " + newParent + " not found");
        }
        E edge = neighbors.getIncomingEdge();
        N oldParent = getParent(node);
        Neighbors<N,E> oldParentNeighbors = nodes.get(oldParent);
        oldParentNeighbors.children.remove(node);
        newParentNeighbors.children.put(node, edge);
        neighbors.setParentNode(newParent);
        edges.put(edge, new Connection<N>(newParent, node));
    }

    public Set<N> getChildren(N node) {
        Neighbors<N,E> neighbors = nodes.get(node);
        if (neighbors == null) {
            throw new ABCDException("Node " + node + " not found");
        }
        return neighbors.getChildren().keySet();
    }

    public int getChildrenCount(N node) {
        Neighbors<N,E> neighbors = nodes.get(node);
        if (neighbors == null) {
            throw new ABCDException("Node " + node + " not found");
        }
        return neighbors.getChildren().size();
    }

    public N getEdgeSource(E edge) {
        if (edge == null) {
            throw new ABCDException("edge == null");
        }
        Connection<N> connection = edges.get(edge);
        if (connection == null) {
            throw new ABCDException("Edge " + edge + " not found");
        }
        return connection.getSource();
    }

    public N getEdgeTarget(E edge) {
        if (edge == null) {
            throw new ABCDException("edge == null");
        }
        Connection<N> connection = edges.get(edge);
        if (connection == null) {
            throw new ABCDException("Edge " + edge + " not found");
        }
        return connection.getTarget();
    }

    public int getDepthFromRoot(N node) {
        Neighbors<N,E> neighbors = nodes.get(node);
        if (neighbors == null) {
            throw new ABCDException("Node " + node + " not found");
        }
        int depth = 0;
        for (N n = getParent(node); n != null; n = getParent(n)) {
            depth++;
        }
        return depth;
    }

    public Tree<N, E> getSubTree(N node) {
        MutableTree<N, E> subTree = new TreeImpl<N, E>(node);
        buildSubTree(node, subTree);
        return subTree;
    }

    private void buildSubTree(N node, MutableTree<N, E> subTree) {
        for (N child : getChildren(node)) {
            subTree.addNode(node, child, getIncomingEdge(child));
        }
        for (N child : getChildren(node)) {
            buildSubTree(child, subTree);
        }
    }

    public Iterator<N> iterator(N node) {
        return getSubTree(root).getNodes().iterator();
    }

    public Iterator<N> iterator() {
        return iterator(root);
    }

    public void writeDOT(Writer writer, String name) throws IOException {
        writeDOT(writer, name, new DefaultDOTAttributeFactory<N>(),
                               new DefaultDOTAttributeFactory<E>());
    }

     public void writeDOT(Writer writer, String name,
                          DOTAttributeFactory<N> nodeAttrFactory,
                          DOTAttributeFactory<E> edgeAttrFactory) throws IOException {
        writer.append("digraph ").append(name).append(" {\n");
        for (E edge : getEdges()) {
            N source = getEdgeSource(edge);
            N target = getEdgeTarget(edge);
            DirectedGraphs.writeEdgeDOT(writer, edge, source, target, edgeAttrFactory);
        }
        for (N node : getNodes()) {
            DirectedGraphs.writeVertexDOT(writer, node, nodeAttrFactory);
        }
        writer.append("}");
    }

}
