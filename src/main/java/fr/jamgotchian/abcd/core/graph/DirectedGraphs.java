/*
 *
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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DirectedGraphs {

    private DirectedGraphs() {
    }

    private static class UnmodifiableDirectedGraph<V ,E> implements DirectedGraph<V, E> {

        private final DirectedGraph<V, E> delegate;

        private UnmodifiableDirectedGraph(DirectedGraph<V, E> delegate) {
            this.delegate = delegate;
        }

        public String toString(E edge) {
            return delegate.toString(edge);
        }

        public String toString(Collection<E> edges) {
            return delegate.toString(edges);
        }

        public void reversePostOrderDFS(V v, Set<V> visited, List<V> vertices, List<E> edges, boolean invert) {
            delegate.reversePostOrderDFS(v, visited, vertices, edges, invert);
        }

        public void reversePostOrderDFS(V v, List<V> vertices, List<E> edges, boolean invert) {
            delegate.reversePostOrderDFS(v, vertices, edges, invert);
        }

        public Set<V> getVertices() {
            return delegate.getVertices();
        }

        public int getVertexCount() {
            return delegate.getVertexCount();
        }

        public Set<V> getSuccessorsOf(V vertex) {
            return delegate.getSuccessorsOf(vertex);
        }

        public int getSuccessorCountOf(V vertex) {
            return delegate.getSuccessorCountOf(vertex);
        }

        public Tree<V, E> getReversePostOrderDFST(V root, boolean invert) {
            return delegate.getReversePostOrderDFST(root, invert);
        }

        public Set<V> getPredecessorsOf(V vertex) {
            return delegate.getPredecessorsOf(vertex);
        }

        public int getPredecessorCountOf(V vertex) {
            return delegate.getPredecessorCountOf(vertex);
        }

        public Collection<E> getOutgoingEdgesOf(V vertex) {
            return delegate.getOutgoingEdgesOf(vertex);
        }

        public V getFirstSuccessorsOf(V vertex) {
            return delegate.getFirstSuccessorsOf(vertex);
        }

        public V getFirstPredecessorsOf(V vertex) {
            return delegate.getFirstPredecessorsOf(vertex);
        }

        public E getFirstOutgoingEdgesOf(V vertex) {
            return delegate.getFirstOutgoingEdgesOf(vertex);
        }

        public E getFirstIncomingEdgesOf(V vertex) {
            return delegate.getFirstIncomingEdgesOf(vertex);
        }

        public Collection<E> getIncomingEdgesOf(V vertex) {
            return delegate.getIncomingEdgesOf(vertex);
        }

        public Set<E> getEdges() {
            return delegate.getEdges();
        }

        public V getEdgeTarget(E edge) {
            return delegate.getEdgeTarget(edge);
        }

        public V getEdgeSource(E edge) {
            return delegate.getEdgeSource(edge);
        }

        public int getEdgeCount() {
            return delegate.getEdgeCount();
        }

        public E getEdge(V source, V target) {
            return delegate.getEdge(source, target);
        }

        public boolean containsEdge(V source, V target) {
            return delegate.containsEdge(source, target);
        }
    }

    public static <V, E> MutableDirectedGraph<V, E> newDirectedGraph() {
        return new DirectedGraphImpl<V, E>();
    }

    public static <V, E> DirectedGraph<V, E> unmodifiableDirectedGraph(DirectedGraph<V, E> graph) {
        return new UnmodifiableDirectedGraph<V, E>(graph);
    }
    
    public static <V, E> String toString(DirectedGraph<V, E> graph, V v) {
        StringBuilder builder = new StringBuilder();
        builder.append("Vertices :\n");
        for (Iterator<V> it = graph.getReversePostOrderDFST(v, false).iterator(); it.hasNext();) {
            builder.append("  ").append(it.next());
            if (it.hasNext()) {
                builder.append("\n");
            }
        }
        builder.append("\n");
        builder.append("Edges :\n");
        for (Iterator<E> it = graph.getEdges().iterator(); it.hasNext();) {
            E e = it.next();
            builder.append("  ").append(e).append(" : ").append(graph.toString(e));
            if (it.hasNext()) {
                builder.append("\n");
            }
        }
        return builder.toString();
    }
}
