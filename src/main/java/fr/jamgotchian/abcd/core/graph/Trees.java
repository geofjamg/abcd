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

package fr.jamgotchian.abcd.core.graph;

import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Trees {

    private Trees() {
    }

    private static class UnmodifiableTree<N, E> implements Tree<N, E> {

        private final Tree<N, E> delegate;

        private UnmodifiableTree(Tree<N, E> delegate) {
            this.delegate = delegate;
        }

        @Override
        public Tree<N, E> getSubTree(N node) {
            return delegate.getSubTree(node);
        }

        @Override
        public N getRoot() {
            return delegate.getRoot();
        }

        @Override
        public N getParent(N node) {
            return delegate.getParent(node);
        }

        @Override
        public Collection<N> getAncestors(N node) {
            return delegate.getAncestors(node);
        }

        @Override
        public Set<N> getNodes() {
            return delegate.getNodes();
        }

        @Override
        public int getNodeCount() {
            return delegate.getNodeCount();
        }

        @Override
        public E getIncomingEdge(N node) {
            return delegate.getIncomingEdge(node);
        }

        @Override
        public Set<E> getEdges() {
            return delegate.getEdges();
        }

        @Override
        public N getEdgeTarget(E edge) {
            return delegate.getEdgeTarget(edge);
        }

        @Override
        public N getEdgeSource(E edge) {
            return delegate.getEdgeSource(edge);
        }

        @Override
        public int getDepthFromRoot(N node) {
            return delegate.getDepthFromRoot(node);
        }

        @Override
        public Set<N> getChildren(N node) {
            return delegate.getChildren(node);
        }

        @Override
        public int getChildrenCount(N node) {
            return delegate.getChildrenCount(node);
        }

        @Override
        public Set<N> getLeaves() {
            return delegate.getLeaves();
        }

        @Override
        public boolean containsNode(N node) {
            return delegate.containsNode(node);
        }

        @Override
        public Iterator<N> iterator(N node) {
            return delegate.iterator(node);
        }

        @Override
        public Iterator<N> iterator() {
            return delegate.iterator();
        }

        @Override
        public List<N> getNodesPostOrder() {
            return delegate.getNodesPostOrder();
        }

        @Override
        public N getFirstCommonAncestor(Collection<N> nodes) {
            return delegate.getFirstCommonAncestor(nodes);
        }

        @Override
        public String getClusterID() {
            return delegate.getClusterID();
        }

        @Override
        public void export(String fileName, String name,
                           GraphvizRenderer<N> nodeRenderer,
                           GraphvizRenderer<E> edgeRenderer) {
            delegate.export(fileName, name, nodeRenderer, edgeRenderer);
        }

        @Override
        public void export(String fileName, String name) {
            delegate.export(fileName, name);
        }

        @Override
        public void export(Writer writer, String name) throws IOException {
            delegate.export(writer, name);
        }

        @Override
        public void export(Writer writer, String name,
                           GraphvizRenderer<N> nodeRenderer,
                           GraphvizRenderer<E> edgeRenderer) throws IOException {
            delegate.export(writer, name, nodeRenderer, edgeRenderer);
        }

        @Override
        public void export(Writer writer, String name,
                           GraphvizRenderer<N> nodeRenderer,
                           GraphvizRenderer<E> edgeRenderer,
                           boolean isSubgraph) throws IOException {
            delegate.export(writer, name, nodeRenderer, edgeRenderer, isSubgraph);
        }
    }

    public static <N, E> MutableTree<N, E> newTree(N root) {
        return new TreeImpl<N, E>(root);
    }

    public static <N, E> Tree<N, E> unmodifiableTree(Tree<N, E> tree) {
        return new UnmodifiableTree<N, E>(tree);
    }

    public static <N, E> String toString(Tree<N, E> tree) {
        StringBuilder builder = new StringBuilder();
        print(tree, tree.getRoot(), builder, 0);
        return builder.toString();
    }

    private static <N, E> void print(Tree<N, E> tree, N node, StringBuilder builder, int indent) {
        for (int i = 0; i < indent * 4; i++) {
            builder.append(" ");
        }
        builder.append(node);
        for (N child : tree.getChildren(node)) {
            builder.append("\n");
            print(tree, child, builder, indent+1);
        }
    }

    public static <N extends Orderable, E> Matrix<Boolean> calculateAncestorsMatrix(Tree<N, E> tree) {
        int nodeCount = tree.getNodes().size();
        Matrix<Boolean> ancestorsMatrix = new Matrix<Boolean>(nodeCount, nodeCount, Boolean.FALSE);

        for (E edge : tree.getEdges()) {
            N source = tree.getEdgeSource(edge);
            N target = tree.getEdgeTarget(edge);
            ancestorsMatrix.setValue(source.getOrder(), target.getOrder(), Boolean.TRUE);
        }

        // compute the transitive closure of the ancestor matrix
        for(int i = 0; i < nodeCount; i++) {
            for(int j = 0; j < nodeCount; j++) {
                if(Boolean.TRUE.equals(ancestorsMatrix.getValue(i, j))) {
                    for(int k = 0; k < nodeCount; k++) {
                        if(Boolean.TRUE.equals(ancestorsMatrix.getValue(j, k))) {
                            ancestorsMatrix.setValue(i, k, Boolean.TRUE);
                        }
                    }
                }
            }
        }

        return ancestorsMatrix;
    }
}
