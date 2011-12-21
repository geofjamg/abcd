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

import fr.jamgotchian.abcd.core.util.Collections3;
import java.util.TreeSet;
import java.util.Arrays;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DirectedGraphImplTest {

    private MutableDirectedGraph<Vertex, String> graph;

    public DirectedGraphImplTest() {
    }

    @Before
    public void setUp() {
        graph = new DirectedGraphImpl<Vertex, String>();

    }

    @After
    public void tearDown() {
        graph = null;
    }

    @Test
    public void testEmptyGraph() {
        assertTrue(graph.getEdges().isEmpty());
        assertTrue(graph.getVertices().isEmpty());
    }

    @Test
    public void testOneVertexGraph() {
        Vertex v0 = new Vertex("v", 0);
        graph.addVertex(v0);
        assertTrue(graph.getEdges().isEmpty());
        assertArrayEquals(Arrays.asList(v0).toArray(), graph.getVertices().toArray());
    }

    @Test
    public void testTwoVertices() {
        Vertex v0 = new Vertex("v", 0);
        Vertex v1 = new Vertex("v", 1);
        String e01 = "e01";
        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addEdge(v0, v1, e01);
        assertArrayEquals(Arrays.asList(v0, v1).toArray(), new TreeSet<Vertex>(graph.getVertices()).toArray());
        assertArrayEquals(Arrays.asList(e01).toArray(), graph.getEdges().toArray());
        assertTrue(graph.getEdge(v0, v1).equals(e01));
        assertArrayEquals(graph.getPredecessorsOf(v1).toArray(), Arrays.asList(v0).toArray());
        assertTrue(graph.getOutgoingEdgesOf(v1).isEmpty());
        assertArrayEquals(graph.getSuccessorsOf(v0).toArray(), Arrays.asList(v1).toArray());
        assertTrue(graph.getIncomingEdgesOf(v0).isEmpty());
    }

    @Test
    public void testFiveVertices() {
        Vertex v0 = new Vertex("v", 0);
        Vertex v1 = new Vertex("v", 1);
        Vertex v2 = new Vertex("v", 2);
        Vertex v3 = new Vertex("v", 3);
        Vertex v4 = new Vertex("v", 4);
        String e01 = "e01";
        String e12 = "e12";
        String e23 = "e23";
        String e24 = "e24";
        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addVertex(v3);
        graph.addVertex(v4);
        graph.addEdge(v0, v1, e01);
        graph.addEdge(v1, v2, e12);
        graph.addEdge(v2, v3, e23);
        graph.addEdge(v2, v4, e24);

        assertArrayEquals(graph.getSuccessorsOf(v2).toArray(), Arrays.asList(v3, v4).toArray());
        assertArrayEquals(graph.getPredecessorsOf(v2).toArray(), Arrays.asList(v1).toArray());
        graph.removeEdge(e12);
        assertArrayEquals(graph.getSuccessorsOf(v2).toArray(), Arrays.asList(v3, v4).toArray());
        assertTrue(graph.getPredecessorsOf(v2).isEmpty());
    }

    @Test
    public void testLoopGraph() {
        Vertex v0 = new Vertex("v", 0);
        Vertex v1 = new Vertex("v", 1);
        String e01 = "e01";
        String e10 = "e10";
        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addEdge(v0, v1, e01);
        graph.addEdge(v1, v0, e10);
        assertArrayEquals(Arrays.asList(v0, v1).toArray(), new TreeSet<Vertex>(graph.getVertices()).toArray());
        assertArrayEquals(Arrays.asList(e01, e10).toArray(), new TreeSet<String>(graph.getEdges()).toArray());
        assertArrayEquals(graph.getSuccessorsOf(v0).toArray(), Arrays.asList(v1).toArray());
        assertArrayEquals(graph.getPredecessorsOf(v1).toArray(), Arrays.asList(v0).toArray());
        assertArrayEquals(graph.getSuccessorsOf(v1).toArray(), Arrays.asList(v0).toArray());
        assertArrayEquals(graph.getPredecessorsOf(v0).toArray(), Arrays.asList(v1).toArray());
    }

    @Test
    public void testSelfLoopGraph() {
        Vertex v0 = new Vertex("v", 0);
        String e00 = "e00";
        graph.addVertex(v0);
        graph.addEdge(v0, v0, e00);
        assertArrayEquals(Arrays.asList(v0).toArray(), graph.getVertices().toArray());
        assertArrayEquals(Arrays.asList(e00).toArray(), graph.getEdges().toArray());
        assertArrayEquals(graph.getSuccessorsOf(v0).toArray(), Arrays.asList(v0).toArray());
        assertArrayEquals(graph.getPredecessorsOf(v0).toArray(), Arrays.asList(v0).toArray());
    }

    @Test
    public void testSelfLoopGraph2() {
        Vertex v0 = new Vertex("v", 0);
        Vertex v1 = new Vertex("v", 1);
        Vertex v2 = new Vertex("v", 2);
        String e01 = "e01";
        String e12 = "e12";
        String e11 = "e11";
        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addVertex(v2);
        graph.addEdge(v0, v1, e01);
        graph.addEdge(v1, v2, e12);
        graph.addEdge(v1, v1, e11);
        assertTrue(Collections3.equals(Arrays.asList(v0, v1, v2), graph.getVertices()));
        assertTrue(Collections3.equals(Arrays.asList(e01, e12, e11), graph.getEdges()));
        assertTrue(graph.getPredecessorsOf(v0).isEmpty());
        assertTrue(graph.getIncomingEdgesOf(v0).isEmpty());
        assertTrue(Collections3.equals(Arrays.asList(v1), graph.getSuccessorsOf(v0)));
        assertTrue(Collections3.equals(Arrays.asList(e01), graph.getOutgoingEdgesOf(v0)));
        assertTrue(Collections3.equals(Arrays.asList(v0, v1), graph.getPredecessorsOf(v1)));
        assertTrue(Collections3.equals(Arrays.asList(e01, e11), graph.getIncomingEdgesOf(v1)));
        assertTrue(Collections3.equals(Arrays.asList(v1, v2), graph.getSuccessorsOf(v1)));
        assertTrue(Collections3.equals(Arrays.asList(e12, e11), graph.getOutgoingEdgesOf(v1)));
        assertTrue(graph.getSuccessorCountOf(v1) == 2);
        assertTrue(graph.getPredecessorCountOf(v1) == 2);
        assertTrue(Collections3.equals(Arrays.asList(v1), graph.getPredecessorsOf(v2)));
        assertTrue(Collections3.equals(Arrays.asList(e12), graph.getIncomingEdgesOf(v2)));
        assertTrue(graph.getSuccessorsOf(v2).isEmpty());
        assertTrue(graph.getOutgoingEdgesOf(v2).isEmpty());
    }

    @Test
    public void testParallelEdges() {
        Vertex v0 = new Vertex("v", 0);
        Vertex v1 = new Vertex("v", 1);
        String e01_1 = "e01_1";
        String e01_2 = "e01_2";
        graph.addVertex(v0);
        graph.addVertex(v1);
        graph.addEdge(v0, v1, e01_1);
        graph.addEdge(v0, v1, e01_2);
        assertTrue(graph.getVertexCount() == 2);
        assertTrue(graph.getEdgeCount() == 2);
        assertTrue(Collections3.equals(Arrays.asList(v0, v1), graph.getVertices()));
        assertTrue(Collections3.equals(Arrays.asList(e01_1, e01_2), graph.getEdges()));
    }

}