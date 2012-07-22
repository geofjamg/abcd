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

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.util.Collections3;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class PostDominatorInfo<N, E> {

    private static final Logger LOGGER = LoggerFactory.getLogger(PostDominatorInfo.class);

    private final DirectedGraph<N, E> graph;

    private final N exitNode;

    private final EdgeFactory<E> edgeFactory;

    private final VertexFactory<N> virtualExitNodeFactory;

    private DirectedGraph<N, E> singleExitGraph;

    private N uniqueExitNode;

    private Map<N, Set<N>> postDominatorsOf;

    private Map<N, N> immediatePostDominator;

    private MutableTree<N, E> postDominatorsTree;

    private Map<N, Set<E>> postDominanceFrontierOf;

    public static <N, E> PostDominatorInfo<N, E>
            create(DirectedGraph<N, E> graph, N exitNode, EdgeFactory<E> edgeFactory,
                   VertexFactory<N> virtualExitNodeFactory) {
        PostDominatorInfo<N, E> domInfo
                = new PostDominatorInfo<N, E>(graph, exitNode, edgeFactory, virtualExitNodeFactory);
        domInfo.update();
        return domInfo;
    }

    private PostDominatorInfo(DirectedGraph<N, E> graph, N exitNode,
            EdgeFactory<E> edgeFactory, VertexFactory<N> virtualExitNodeFactory) {
        this.graph = graph;
        this.exitNode = exitNode;
        this.edgeFactory = edgeFactory;
        this.virtualExitNodeFactory = virtualExitNodeFactory;
    }

    public Set<N> getPostDominatorsOf(N n) {
        return Collections.unmodifiableSet(postDominatorsOf.get(n));
    }

    public Tree<N, E> getPostDominatorsTree() {
        return Trees.unmodifiableTree(postDominatorsTree);
    }

    public Set<E> getPostDominanceFrontierOf(N n) {
        return Collections.unmodifiableSet(postDominanceFrontierOf.get(n));
    }

    public Set<N> getPostDominanceFrontierOf2(N n) {
        Set<N> frontier = new HashSet<N>();
        for (E e : postDominanceFrontierOf.get(n)) {
            frontier.add(singleExitGraph.getEdgeSource(e));
        }
        return frontier;
    }

    public boolean postDominates(N x, N y) {
        return postDominatorsOf.get(y).contains(x);
    }

    public boolean strictlyPostDominates(N x, N y) {
        return !x.equals(y) && postDominates(x, y);
    }

    public N getImmediatePostDominatorOf(N n) {
        return immediatePostDominator.get(n);
    }

    private void computeImmediatePostDominators() {
        // the immediate post dominator of a node n is obtained by choosing the
        // strict post dominator of n whose post dominator set differs from that
        // of n only by removal of n.
        immediatePostDominator = new HashMap<N, N>();
        for (Map.Entry<N, Set<N>> entry : postDominatorsOf.entrySet()) {
            N n = entry.getKey();
            if (!n.equals(uniqueExitNode)) {
                Set<N> postDominators = entry.getValue();
                boolean found = false;
                for (N pd : postDominators) {
                    if (!pd.equals(n)
                            && Collections3.equals(Sets.union(postDominatorsOf.get(pd),
                                                              Collections.singleton(n)),
                                                   postDominators)) {
                        immediatePostDominator.put(n, pd);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    LOGGER.warn("Cannot find immediate post dominator of {}",  n);
                }
            }
        }
        LOGGER.trace("Immediate post dominators {}", immediatePostDominator);
    }

    private void computePostDominanceFrontier() {
        postDominanceFrontierOf = new HashMap<N, Set<E>>();

        for (N x : singleExitGraph.getVertices()) {
            postDominanceFrontierOf.put(x, new HashSet<E>());
            for (N y : postDominatorsTree.getSubTree(x).getNodes()) {
                for (N z : singleExitGraph.getPredecessorsOf(y)) {
                    if (!z.equals(x)) {
                        if (!strictlyPostDominates(x, z)) {
                            E zy = singleExitGraph.getEdge(z, y);
                            postDominanceFrontierOf.get(x).add(zy);
                        }
                    }
                }
            }

            LOGGER.trace("Post dominance frontier of {} : {}",
                    x, singleExitGraph.toString(postDominanceFrontierOf.get(x)));
       }
    }

    public void update() {
        LOGGER.debug("Update post dominator info");

        int exitCount = graph.getExits().size();
        if (exitCount == 1) {
            singleExitGraph = graph;
            uniqueExitNode = exitNode;
        } else if (exitCount > 1) {
            LOGGER.trace("Multi exits graph, create a virtual exit node");
            MutableDirectedGraph<N, E> newGraph = DirectedGraphs.newDirectedGraph(graph);
            uniqueExitNode = virtualExitNodeFactory.createVertex();
            newGraph.addVertex(uniqueExitNode);
            for (N n : newGraph.getExits()) {
                newGraph.addEdge(n, uniqueExitNode, edgeFactory.createEdge());
            }
            singleExitGraph = newGraph;
        } else {
            throw new ABCDException("Cannot compute post dominator infos with a "
                    + exitCount + " exit(s) graph");
        }

        // find post-dominators
        postDominatorsOf = new PostDominatorsFinder<N, E>(singleExitGraph, uniqueExitNode).analyse();
        for (Map.Entry<N, Set<N>> entry : postDominatorsOf.entrySet()) {
            LOGGER.trace("Post-dominators of {} : {}", entry.getKey(), entry.getValue());
        }

        computeImmediatePostDominators();

        // build post-dominators tree
        postDominatorsTree = Trees.newTree(uniqueExitNode);
        Multimap<N, N> children = HashMultimap.create();
        for (Map.Entry<N, N> entry : immediatePostDominator.entrySet()) {
            children.put(entry.getValue(), entry.getKey());
        }
        DominatorInfo.buildTree(uniqueExitNode, postDominatorsTree, children, edgeFactory);
        LOGGER.trace("Post dominators tree :\n{}", Trees.toString(postDominatorsTree));

        // compute post dominance frontier
        computePostDominanceFrontier();
    }
}
