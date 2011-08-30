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

import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.DOTAttributeFactory;
import fr.jamgotchian.abcd.core.graph.DOTExportable;
import fr.jamgotchian.abcd.core.graph.DirectedGraphs;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;

/**
 * Represent a single entry, single exit region.
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RegionGraph implements DOTExportable<Region, Edge> {

    private final MutableDirectedGraph<Region, Edge> graph;

    private Region entry;

    private Region exit;

    public RegionGraph() {
        graph = DirectedGraphs.newDirectedGraph();
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

    public Collection<Region> getRegions() {
        return graph.getVertices();
    }

    public Collection<Edge> getEdges() {
        return graph.getEdges();
    }

    public void writeDOT(Writer writer, String name,
                         DOTAttributeFactory<Region> vertexAttrFactory,
                         DOTAttributeFactory<Edge> edgeAttrFactory) throws IOException {
        graph.writeDOT(writer, name, vertexAttrFactory, edgeAttrFactory);
    }
}
