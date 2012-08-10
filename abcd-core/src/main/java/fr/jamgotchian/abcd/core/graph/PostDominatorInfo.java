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
import fr.jamgotchian.abcd.core.util.Collections3;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
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

    private Map<N, Set<N>> postDominators;

    private Map<N, N> immediatePostDominator;

    private Map<N, Set<N>> multiExitsImmediatePostDominator;

    private MutableTree<N, E> postDominatorsTree;

    private Map<N, Set<E>> postDominanceFrontierOf;

    public static <N, E> PostDominatorInfo<N, E>
            create(DirectedGraph<N, E> graph, N exitNode, EdgeFactory<E> edgeFactory) {
        PostDominatorInfo<N, E> domInfo
                = new PostDominatorInfo<N, E>(graph, exitNode, edgeFactory);
        domInfo.update();
        return domInfo;
    }

    private PostDominatorInfo(DirectedGraph<N, E> graph, N exitNode,
            EdgeFactory<E> edgeFactory) {
        this.graph = graph;
        this.exitNode = exitNode;
        this.edgeFactory = edgeFactory;
    }

    public Set<N> getPostDominatorsOf(N n) {
        return Collections.unmodifiableSet(postDominators.get(n));
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
            frontier.add(graph.getEdgeSource(e));
        }
        return frontier;
    }

    public boolean postDominates(N x, N y) {
        return postDominators.get(y).contains(x);
    }

    public boolean strictlyPostDominates(N x, N y) {
        return !x.equals(y) && postDominates(x, y);
    }

    public N getImmediatePostDominatorOf(N n) {
        return immediatePostDominator.get(n);
    }

    public Set<N> getMultiExitsImmediatePostDominatorOf(N n) {
        return multiExitsImmediatePostDominator.get(n);
    }

    private static <N> Map<N, N> computeImmediatePostDominators(Map<N, Set<N>> postDominators, N exitNode) {
        // the immediate post dominator of a node n is obtained by choosing the
        // strict post dominator of n whose post dominator set differs from that
        // of n only by removal of n.
        Map<N, N> immediatePostDominator = new HashMap<N, N>();
        for (Map.Entry<N, Set<N>> entry : postDominators.entrySet()) {
            N n = entry.getKey();
            Set<N> pdn = entry.getValue(); // post dominators of n
            if (pdn != null && !n.equals(exitNode)) {
                boolean found = false;
                for (N p : pdn) {
                    if (!p.equals(n)
                            && Collections3.equals(Sets.union(postDominators.get(p),
                                                              Collections.singleton(n)),
                                                   pdn)) {
                        immediatePostDominator.put(n, p);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    LOGGER.warn("Cannot find immediate post dominator of {}",  n);
                }
            }
        }
        return immediatePostDominator;
    }

    private void computePostDominanceFrontier() {
        postDominanceFrontierOf = new HashMap<N, Set<E>>();

        for (N x : postDominatorsTree.getNodes()) {
            postDominanceFrontierOf.put(x, new HashSet<E>());
            for (N y : postDominatorsTree.getSubTree(x).getNodes()) {
                for (N z : graph.getPredecessorsOf(y)) {
                    if (!z.equals(x)) {
                        if (!strictlyPostDominates(x, z)) {
                            E zy = graph.getEdge(z, y);
                            postDominanceFrontierOf.get(x).add(zy);
                        }
                    }
                }
            }

            LOGGER.trace("Post dominance frontier of {} : {}",
                    x, graph.toString(postDominanceFrontierOf.get(x)));
       }
    }

    public void update() {
        LOGGER.debug("Update post dominator info");

        // find post-dominators
        postDominators = new PostDominatorsFinder<N, E>(graph, exitNode).analyse();
        for (Map.Entry<N, Set<N>> entry : postDominators.entrySet()) {
            LOGGER.trace("Post-dominators of {} : {}", entry.getKey(), entry.getValue());
        }

        // compute immediate post dominators
        immediatePostDominator = computeImmediatePostDominators(postDominators, exitNode);
        LOGGER.trace("Immediate post dominators {}", immediatePostDominator);

        // immediate post dominators for multi exits graph
        multiExitsImmediatePostDominator = new HashMap<N, Set<N>>();
        for (N n : graph.getVertices()) {
            multiExitsImmediatePostDominator.put(n, new LinkedHashSet<N>());
        }
        for (Map.Entry<N, N> entry : immediatePostDominator.entrySet()) {
            multiExitsImmediatePostDominator.get(entry.getKey()).add(entry.getValue());
        }
        Set<N> otherExitNodes = new HashSet<N>(graph.getExits());
        otherExitNodes.remove(exitNode);
        if (otherExitNodes.size() > 0) {
            for (N otherExitNode : otherExitNodes) {
                Map<N, Set<N>> otherPostDominators
                        = new PostDominatorsFinder<N, E>(graph, otherExitNode).analyse();
                Map<N, N> otherImmediatePostDominator
                        = computeImmediatePostDominators(otherPostDominators, otherExitNode);
                LOGGER.trace("Immediate post dominators for exit {}:{}",
                        otherExitNode, otherImmediatePostDominator);

                for (Map.Entry<N, N> entry : otherImmediatePostDominator.entrySet()) {
                    multiExitsImmediatePostDominator.get(entry.getKey()).add(entry.getValue());
                }
            }
        }
        LOGGER.trace("Multi exits immediate post dominators {}", multiExitsImmediatePostDominator);

        // build post-dominators tree
        postDominatorsTree = Trees.newTree(exitNode, immediatePostDominator, edgeFactory);
        LOGGER.trace("Post dominators tree :\n{}", Trees.toString(postDominatorsTree));

        // compute post dominance frontier
        computePostDominanceFrontier();
    }
}
