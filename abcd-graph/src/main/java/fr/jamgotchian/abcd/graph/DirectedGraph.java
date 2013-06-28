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

package fr.jamgotchian.abcd.graph;

import com.google.common.base.Predicate;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface DirectedGraph<V, E> {

    boolean containsEdge(V source, V target);

    boolean containsEdge(E edge);

    E getEdge(V source, V target);

    V getEdgeSource(E edge);

    V getEdgeTarget(E edge);

    Set<E> getEdges();

    int getEdgeCount();

    Collection<E> getIncomingEdgesOf(V vertex, Predicate<E> filter);

    Collection<E> getIncomingEdgesOf(V vertex);

    E getFirstIncomingEdgeOf(V vertex, Predicate<E> filter);

    E getFirstIncomingEdgeOf(V vertex);

    Collection<E> getOutgoingEdgesOf(V vertex, Predicate<E> filter);

    Collection<E> getOutgoingEdgesOf(V vertex);

    E getFirstOutgoingEdgeOf(V vertex, Predicate<E> filter);

    E getFirstOutgoingEdgeOf(V vertex);

    Set<V> getPredecessorsOf(V vertex, Predicate<E> filter);

    Set<V> getPredecessorsOf(V vertex);

    V getFirstPredecessorOf(V vertex, Predicate<E> filter);

    V getFirstPredecessorOf(V vertex);

    int getPredecessorCountOf(V vertex, Predicate<E> filter);

    int getPredecessorCountOf(V vertex);

    Set<V> getSuccessorsOf(V vertex, Predicate<E> filter);

    Set<V> getSuccessorsOf(V vertex);

    V getFirstSuccessorOf(V vertex, Predicate<E> filter);

    V getFirstSuccessorOf(V vertex);

    int getSuccessorCountOf(V vertex, Predicate<E> filter);

    int getSuccessorCountOf(V vertex);

    boolean containsVertex(V vertex);

    Set<V> getVertices();

    int getVertexCount();

    Set<V> getEntries();

    Set<V> getExits();

    void reversePostOrderDFS(V v, List<V> vertices, List<E> edges, boolean invert);

    void reversePostOrderDFS(V v, Set<V> visited, List<V> vertices, List<E> edges, boolean invert);

    Tree<V, E> getReversePostOrderDFST(V root, Set<V> visited, boolean invert);

    Tree<V, E> getReversePostOrderDFST(V root, boolean invert);

    void exportPane(Writer writer, String title, int paneId, int indentLevel,
                    GraphvizRenderer<V> vertexRenderer,
                    GraphvizRenderer<E> edgeRenderer) throws IOException;

    void export(Writer writer, String title, GraphvizRenderer<V> vertexRenderer,
                GraphvizRenderer<E> edgeRenderer) throws IOException;

    void export(Writer writer, String title) throws IOException;

    void export(String fileName, String title) throws IOException;

    String toString(Collection<E> edges);

    String toString(E edge);

}
