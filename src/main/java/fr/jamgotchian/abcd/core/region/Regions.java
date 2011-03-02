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
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Regions {

    private Regions() {
    }

    public static Collection<Edge> getOutgoingEdgesOf(DirectedGraph<Region, Edge> graph, 
                                                      Region region, boolean exceptional) {
        List<Edge> outgoingEdges = new ArrayList<Edge>();
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                outgoingEdges.add(edge);              
            }
        }
        return outgoingEdges;
    }

    public static Collection<Edge> getIncomingEdgesOf(DirectedGraph<Region, Edge> graph, 
                                                      Region region, boolean exceptional) {
        List<Edge> incomingEdges = new ArrayList<Edge>();
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                incomingEdges.add(edge);
            }
        }
        return incomingEdges;
    }

    public static Collection<Region> getSuccessorsOf(DirectedGraph<Region, Edge> graph, 
                                                     Region region, boolean exceptional) {
        List<Region> successors = new ArrayList<Region>();
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                successors.add(graph.getEdgeTarget(edge));
            }
        }
        return successors;
    }

    public static Collection<Region> getPredecessorsOf(DirectedGraph<Region, Edge> graph, 
                                                       Region region, boolean exceptional) {
        List<Region> predecessors = new ArrayList<Region>();
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                predecessors.add(graph.getEdgeSource(edge));
            }
        }
        return predecessors;
    }

    public static Edge getFirstOutgoingEdgeOf(DirectedGraph<Region, Edge> graph, 
                                              Region region, boolean exceptional) {
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                return edge;
            }
        }
        return null;
    }
        
    public static Edge getFirstIncomingEdgeOf(DirectedGraph<Region, Edge> graph, 
                                              Region region, boolean exceptional) {
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                return edge;
            }
        }
        return null;
    }

    public static Region getFirstSuccessorOf(DirectedGraph<Region, Edge> graph, 
                                             Region region, boolean exceptional) {
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                return graph.getEdgeTarget(edge);
            }
        }
        return null;
    }
        
    public static Region getFirstPredecessorOf(DirectedGraph<Region, Edge> graph, 
                                               Region region, boolean exceptional) {
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                return graph.getEdgeSource(edge);
            }
        }
        return null;
    }

    public static int getSuccessorCountOf(DirectedGraph<Region, Edge> graph, 
                                          Region region, boolean exceptional) {
        int count = 0;
        for (Edge edge : graph.getOutgoingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                count++;
            }
        }
        return count;
    }

    public static int getPredecessorCountOf(DirectedGraph<Region, Edge> graph, 
                                            Region region, boolean exceptional) {
        int count = 0;
        for (Edge edge : graph.getIncomingEdgesOf(region)) {
            if ((exceptional && edge.isExceptional()) 
                    || (!exceptional && !edge.isExceptional())) {
                count++;
            }
        }
        return count;
    }

}
