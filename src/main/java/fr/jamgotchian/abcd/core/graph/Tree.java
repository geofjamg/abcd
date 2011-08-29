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
import java.util.Iterator;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface Tree<N, E> extends Iterable<N> {

    boolean containsNode(N node);

    Set<N> getChildren(N node);

    int getDepthFromRoot(N node);

    N getEdgeSource(E edge);

    N getEdgeTarget(E edge);

    Set<E> getEdges();

    E getIncomingEdge(N node);

    int getNodeCount();

    Set<N> getNodes();

    N getParent(N node);

    N getRoot();

    Tree<N, E> getSubTree(N node);

    Iterator<N> iterator(N node);

    void writeDOT(Writer writer, String name) throws IOException;

    void writeDOT(Writer writer, String name, DOTAttributeFactory<N> nodeAttrFactory,
                  DOTAttributeFactory<E> edgeAttrFactory) throws IOException;
}
