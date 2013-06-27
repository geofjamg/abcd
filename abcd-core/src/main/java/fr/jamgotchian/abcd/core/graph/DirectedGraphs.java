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

import com.google.common.base.Predicate;
import fr.jamgotchian.abcd.core.common.ABCDException;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DirectedGraphs {

    private DirectedGraphs() {
    }

    private static class ListenableDirectedGraphImpl<V ,E> implements ListenableMutableDirectedGraph<V, E> {

        private final MutableDirectedGraph<V, E> delegate;

        private final List<DirectedGraphListener> listeners;

        private ListenableDirectedGraphImpl(MutableDirectedGraph<V, E> delegate) {
            this.delegate = delegate;
            listeners = new CopyOnWriteArrayList<>();
        }

        @Override
        public void addListener(DirectedGraphListener l) {
            listeners.add(l);
        }

        @Override
        public void removeListener(DirectedGraphListener l) {
            listeners.remove(l);
        }

        private void notifyListeners() {
            for (DirectedGraphListener l : listeners) {
                l.graphChanged();
            }
        }

        @Override
        public void export(String fileName, String title) {
            delegate.export(fileName, title);
        }

        @Override
        public void export(Writer writer, String title) throws IOException {
            delegate.export(writer, title);
        }

        @Override
        public void export(Writer writer, String title,
                           GraphvizRenderer<V> nodeRenderer,
                           GraphvizRenderer<E> edgeRenderer) throws IOException {
            delegate.export(writer, title, nodeRenderer, edgeRenderer);
        }

        @Override
        public void exportPane(Writer writer, String title, int paneId, int indentLevel,
                               GraphvizRenderer<V> vertexRenderer, GraphvizRenderer<E> edgeRenderer) throws IOException {
            delegate.exportPane(writer, title, paneId, indentLevel, vertexRenderer, edgeRenderer);
        }

        @Override
        public String toString(E edge) {
            return delegate.toString(edge);
        }

        @Override
        public String toString(Collection<E> edges) {
            return delegate.toString(edges);
        }

        @Override
        public void reversePostOrderDFS(V v, Set<V> visited, List<V> vertices, List<E> edges, boolean invert) {
            delegate.reversePostOrderDFS(v, visited, vertices, edges, invert);
        }

        @Override
        public void reversePostOrderDFS(V v, List<V> vertices, List<E> edges, boolean invert) {
            delegate.reversePostOrderDFS(v, vertices, edges, invert);
        }

        @Override
        public Set<V> getVertices() {
            return delegate.getVertices();
        }

        @Override
        public int getVertexCount() {
            return delegate.getVertexCount();
        }

        @Override
        public Set<V> getSuccessorsOf(V vertex) {
            return delegate.getSuccessorsOf(vertex);
        }

        @Override
        public int getSuccessorCountOf(V vertex) {
            return delegate.getSuccessorCountOf(vertex);
        }

        @Override
        public Tree<V, E> getReversePostOrderDFST(V root, Set<V> visited, boolean invert) {
            return delegate.getReversePostOrderDFST(root, visited, invert);
        }

        @Override
        public Tree<V, E> getReversePostOrderDFST(V root, boolean invert) {
            return delegate.getReversePostOrderDFST(root, invert);
        }

        @Override
        public Set<V> getPredecessorsOf(V vertex) {
            return delegate.getPredecessorsOf(vertex);
        }

        @Override
        public int getPredecessorCountOf(V vertex) {
            return delegate.getPredecessorCountOf(vertex);
        }

        @Override
        public Collection<E> getOutgoingEdgesOf(V vertex) {
            return delegate.getOutgoingEdgesOf(vertex);
        }

        @Override
        public Collection<E> getIncomingEdgesOf(V vertex) {
            return delegate.getIncomingEdgesOf(vertex);
        }

        @Override
        public V getFirstSuccessorOf(V vertex) {
            return delegate.getFirstSuccessorOf(vertex);
        }

        @Override
        public V getFirstPredecessorOf(V vertex) {
            return delegate.getFirstPredecessorOf(vertex);
        }

        @Override
        public E getFirstOutgoingEdgeOf(V vertex) {
            return delegate.getFirstOutgoingEdgeOf(vertex);
        }

        @Override
        public E getFirstIncomingEdgeOf(V vertex) {
            return delegate.getFirstIncomingEdgeOf(vertex);
        }

        @Override
        public Set<V> getExits() {
            return delegate.getExits();
        }

        @Override
        public Set<V> getEntries() {
            return delegate.getEntries();
        }

        @Override
        public Set<E> getEdges() {
            return delegate.getEdges();
        }

        @Override
        public V getEdgeTarget(E edge) {
            return delegate.getEdgeTarget(edge);
        }

        @Override
        public V getEdgeSource(E edge) {
            return delegate.getEdgeSource(edge);
        }

        @Override
        public int getEdgeCount() {
            return delegate.getEdgeCount();
        }

        @Override
        public E getEdge(V source, V target) {
            return delegate.getEdge(source, target);
        }

        @Override
        public boolean containsVertex(V vertex) {
            return delegate.containsVertex(vertex);
        }

        @Override
        public boolean containsEdge(E edge) {
            return delegate.containsEdge(edge);
        }

        @Override
        public boolean containsEdge(V source, V target) {
            return delegate.containsEdge(source, target);
        }

        @Override
        public void splitVertex(V vertex, V newVertex) {
            delegate.splitVertex(vertex, newVertex);
            notifyListeners();
        }

        @Override
        public void removeVertex(V vertex) {
            delegate.removeVertex(vertex);
            notifyListeners();
        }

        @Override
        public boolean removeEdge(V source, V target) {
            boolean removed = delegate.removeEdge(source, target);
            notifyListeners();
            return removed;
        }

        @Override
        public void removeEdge(E edge) {
            delegate.removeEdge(edge);
            notifyListeners();
        }

        @Override
        public MutableDirectedGraph<V, E> getSubgraphContaining(V vertex) {
            return delegate.getSubgraphContaining(vertex);
        }

        @Override
        public void addVertex(V vertex) {
            delegate.addVertex(vertex);
            notifyListeners();
        }

        @Override
        public void addEdge(V source, V target, E edge) {
            delegate.addEdge(source, target, edge);
            notifyListeners();
        }

        @Override
        public Collection<E> getIncomingEdgesOf(V vertex, Predicate<E> filter) {
            return delegate.getIncomingEdgesOf(vertex, filter);
        }

        @Override
        public E getFirstIncomingEdgeOf(V vertex, Predicate<E> filter) {
            return delegate.getFirstIncomingEdgeOf(vertex, filter);
        }

        @Override
        public Collection<E> getOutgoingEdgesOf(V vertex, Predicate<E> filter) {
            return delegate.getOutgoingEdgesOf(vertex, filter);
        }

        @Override
        public E getFirstOutgoingEdgeOf(V vertex, Predicate<E> filter) {
            return delegate.getFirstOutgoingEdgeOf(vertex, filter);
        }

        @Override
        public Set<V> getPredecessorsOf(V vertex, Predicate<E> filter) {
            return delegate.getPredecessorsOf(vertex, filter);
        }

        @Override
        public V getFirstPredecessorOf(V vertex, Predicate<E> filter) {
            return delegate.getFirstPredecessorOf(vertex, filter);
        }

        @Override
        public int getPredecessorCountOf(V vertex, Predicate<E> filter) {
            return delegate.getPredecessorCountOf(vertex, filter);
        }

        @Override
        public Set<V> getSuccessorsOf(V vertex, Predicate<E> filter) {
            return delegate.getSuccessorsOf(vertex, filter);
        }

        @Override
        public V getFirstSuccessorOf(V vertex, Predicate<E> filter) {
            return delegate.getFirstSuccessorOf(vertex, filter);
        }

        @Override
        public int getSuccessorCountOf(V vertex, Predicate<E> filter) {
            return delegate.getSuccessorCountOf(vertex, filter);
        }

    }

    private static class UnmodifiableDirectedGraph<V ,E> implements DirectedGraph<V, E> {

        private final DirectedGraph<V, E> delegate;

        private UnmodifiableDirectedGraph(DirectedGraph<V, E> delegate) {
            this.delegate = delegate;
        }

        @Override
        public String toString(E edge) {
            return delegate.toString(edge);
        }

        @Override
        public String toString(Collection<E> edges) {
            return delegate.toString(edges);
        }

        @Override
        public void reversePostOrderDFS(V v, Set<V> visited, List<V> vertices, List<E> edges, boolean invert) {
            delegate.reversePostOrderDFS(v, visited, vertices, edges, invert);
        }

        @Override
        public void reversePostOrderDFS(V v, List<V> vertices, List<E> edges, boolean invert) {
            delegate.reversePostOrderDFS(v, vertices, edges, invert);
        }

        @Override
        public Set<V> getVertices() {
            return delegate.getVertices();
        }

        @Override
        public boolean containsVertex(V vertex) {
            return delegate.containsVertex(vertex);
        }

        @Override
        public int getVertexCount() {
            return delegate.getVertexCount();
        }

        @Override
        public Set<V> getSuccessorsOf(V vertex) {
            return delegate.getSuccessorsOf(vertex);
        }

        @Override
        public int getSuccessorCountOf(V vertex) {
            return delegate.getSuccessorCountOf(vertex);
        }

        @Override
        public Tree<V, E> getReversePostOrderDFST(V root, Set<V> visited, boolean invert) {
            return delegate.getReversePostOrderDFST(root, visited, invert);
        }

        @Override
        public Tree<V, E> getReversePostOrderDFST(V root, boolean invert) {
            return delegate.getReversePostOrderDFST(root, invert);
        }

        @Override
        public Set<V> getPredecessorsOf(V vertex) {
            return delegate.getPredecessorsOf(vertex);
        }

        @Override
        public int getPredecessorCountOf(V vertex) {
            return delegate.getPredecessorCountOf(vertex);
        }

        @Override
        public Collection<E> getOutgoingEdgesOf(V vertex) {
            return delegate.getOutgoingEdgesOf(vertex);
        }

        @Override
        public V getFirstSuccessorOf(V vertex) {
            return delegate.getFirstSuccessorOf(vertex);
        }

        @Override
        public V getFirstPredecessorOf(V vertex) {
            return delegate.getFirstPredecessorOf(vertex);
        }

        @Override
        public E getFirstOutgoingEdgeOf(V vertex) {
            return delegate.getFirstOutgoingEdgeOf(vertex);
        }

        @Override
        public E getFirstIncomingEdgeOf(V vertex) {
            return delegate.getFirstIncomingEdgeOf(vertex);
        }

        @Override
        public Collection<E> getIncomingEdgesOf(V vertex) {
            return delegate.getIncomingEdgesOf(vertex);
        }

        @Override
        public Set<E> getEdges() {
            return delegate.getEdges();
        }

        @Override
        public V getEdgeTarget(E edge) {
            return delegate.getEdgeTarget(edge);
        }

        @Override
        public V getEdgeSource(E edge) {
            return delegate.getEdgeSource(edge);
        }

        @Override
        public int getEdgeCount() {
            return delegate.getEdgeCount();
        }

        @Override
        public E getEdge(V source, V target) {
            return delegate.getEdge(source, target);
        }

        @Override
        public boolean containsEdge(E edge) {
            return delegate.containsEdge(edge);
        }

        @Override
        public boolean containsEdge(V source, V target) {
            return delegate.containsEdge(source, target);
        }

        @Override
        public Set<V> getEntries() {
            return delegate.getEntries();
        }

        @Override
        public Set<V> getExits() {
            return delegate.getExits();
        }

        @Override
        public void export(String fileName, String title) {
            delegate.export(fileName, title);
        }

        @Override
        public void export(Writer writer, String title) throws IOException {
            delegate.export(writer, title);
        }

        @Override
        public void export(Writer writer, String title,
                           GraphvizRenderer<V> vertexRenderer,
                           GraphvizRenderer<E> edgeRenderer) throws IOException {
            delegate.export(writer, title, vertexRenderer, edgeRenderer);
        }

        @Override
        public void exportPane(Writer writer, String title, int paneId, int indentLevel,
                               GraphvizRenderer<V> vertexRenderer, GraphvizRenderer<E> edgeRenderer) throws IOException {
            delegate.exportPane(writer, title, paneId, indentLevel, vertexRenderer, edgeRenderer);
        }

        @Override
        public Collection<E> getIncomingEdgesOf(V vertex, Predicate<E> filter) {
            return delegate.getIncomingEdgesOf(vertex, filter);
        }

        @Override
        public E getFirstIncomingEdgeOf(V vertex, Predicate<E> filter) {
            return delegate.getFirstIncomingEdgeOf(vertex, filter);
        }

        @Override
        public Collection<E> getOutgoingEdgesOf(V vertex, Predicate<E> filter) {
            return delegate.getOutgoingEdgesOf(vertex, filter);
        }

        @Override
        public E getFirstOutgoingEdgeOf(V vertex, Predicate<E> filter) {
            return delegate.getFirstOutgoingEdgeOf(vertex, filter);
        }

        @Override
        public Set<V> getPredecessorsOf(V vertex, Predicate<E> filter) {
            return delegate.getPredecessorsOf(vertex, filter);
        }

        @Override
        public V getFirstPredecessorOf(V vertex, Predicate<E> filter) {
            return delegate.getFirstPredecessorOf(vertex, filter);
        }

        @Override
        public int getPredecessorCountOf(V vertex, Predicate<E> filter) {
            return delegate.getPredecessorCountOf(vertex, filter);
        }

        @Override
        public Set<V> getSuccessorsOf(V vertex, Predicate<E> filter) {
            return delegate.getSuccessorsOf(vertex, filter);
        }

        @Override
        public V getFirstSuccessorOf(V vertex, Predicate<E> filter) {
            return delegate.getFirstSuccessorOf(vertex, filter);
        }

        @Override
        public int getSuccessorCountOf(V vertex, Predicate<E> filter) {
            return delegate.getSuccessorCountOf(vertex, filter);
        }

    }

    public static <V, E> MutableDirectedGraph<V, E> newDirectedGraph() {
        return new DirectedGraphImpl<>();
    }

    public static <V, E> MutableDirectedGraph<V, E> newDirectedGraph(Map<V, Set<V>> sources, EdgeFactory<E> factory) {
        MutableDirectedGraph<V, E> graph = newDirectedGraph();
        for (Map.Entry<V, Set<V>> entry : sources.entrySet()) {
            V t = entry.getKey();
            for (V s : entry.getValue()) {
                if (!graph.containsVertex(s)) {
                    graph.addVertex(s);
                }
                if (!graph.containsVertex(t)) {
                    graph.addVertex(t);
                }
                graph.addEdge(s, t, factory.createEdge());
            }
        }
        return graph;
    }

    public static <V, E> MutableDirectedGraph<V, E> newDirectedGraph(DirectedGraph<V, E> other) {
        return new DirectedGraphImpl<>(other);
    }

    public static <V, E> DirectedGraph<V, E> unmodifiableDirectedGraph(DirectedGraph<V, E> graph) {
        return new UnmodifiableDirectedGraph<>(graph);
    }

    public static <V, E> MutableDirectedGraph<V, E> listenableDirectedGraph(MutableDirectedGraph<V, E> graph) {
        return new ListenableDirectedGraphImpl<>(graph);
    }

    public static <V, E> String toString(DirectedGraph<V, E> graph, String name) {
        StringBuilder builder = new StringBuilder();
        builder.append("digraph ").append(name).append(" {\n");
        for (V v : graph.getVertices()) {
            builder.append("    ").append(v).append("\n");
        }
        for (E e : graph.getEdges()) {
            builder.append("    ").append(graph.toString(e)).append("\n");
        }
        builder.append("}");
        return builder.toString();
    }

    public static <V, E> DirectedGraph<V, E> getSingleExitGraph(DirectedGraph<V, E> graph, V exit) {
        Set<V> exits = graph.getExits();
        if (exits.isEmpty()) {
            throw new ABCDException("No exit");
        }
        if (exits.size() == 1) {
            if (exits.iterator().next() != exit) {
                throw new ABCDException("Wrong exit ");
            }
            return graph;
        }
        Set<V> visited = new HashSet<>();
        graph.getReversePostOrderDFST(exit, visited, true);
        MutableDirectedGraph<V, E> graph2 = newDirectedGraph(graph);
        for (V v : new ArrayList<>(graph2.getVertices())) {
            if (!visited.contains(v)) {
                graph2.removeVertex(v);
            }
        }
        return graph2;
    }

}
