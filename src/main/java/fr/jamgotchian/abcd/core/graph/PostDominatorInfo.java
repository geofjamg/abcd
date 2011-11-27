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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class PostDominatorInfo<N, E> {

    private static final Logger LOGGER = Logger.getLogger(PostDominatorInfo.class.getName());

    private final DirectedGraph<N, E> graph;

    private final N exitNode;

    private final EdgeFactory<E> factory;

    private Map<N, Set<N>> postDominatorsOf;

    private Map<N, N> immediatePostDominator;

    private MutableTree<N, E> postDominatorsTree;

    private Map<N, Set<E>> postDominanceFrontierOf;

    public static <N, E> PostDominatorInfo<N, E>
            create(DirectedGraph<N, E> graph, N exitNode, EdgeFactory<E> factory) {
        PostDominatorInfo<N, E> domInfo = new PostDominatorInfo<N, E>(graph, exitNode, factory);
        domInfo.update();
        return domInfo;
    }

    private PostDominatorInfo(DirectedGraph<N, E> graph, N exitNode, EdgeFactory<E> factory) {
        this.graph = graph;
        this.exitNode = exitNode;
        this.factory = factory;
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
            frontier.add(graph.getEdgeSource(e));
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
            if (!n.equals(exitNode)) {
                Set<N> postDominators = entry.getValue();
                boolean found = false;
                for (N pd : postDominators) {
                    if (!pd.equals(n)
                            && Collections3.sameContent(Sets.union(postDominatorsOf.get(pd),
                                                                   Collections.singleton(n)),
                                                        postDominators)) {
                        immediatePostDominator.put(n, pd);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    LOGGER.log(Level.WARNING, "Cannot find immediate post dominator of {0}",  n);
                }
            }
        }
        LOGGER.log(Level.FINEST, "Immediate post dominators {0}", immediatePostDominator);
    }

    private void computePostDominanceFrontier() {
        postDominanceFrontierOf = new HashMap<N, Set<E>>();

        for (N x : graph.getVertices()) {
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

            LOGGER.log(Level.FINEST, "Post dominance frontier of {0} : {1}",
                    new Object[] {x, graph.toString(postDominanceFrontierOf.get(x))});
       }
    }

    public void update() {
        LOGGER.log(Level.FINER, "Update post dominator info");

        // find post-dominators
        postDominatorsOf = new PostDominatorsFinder<N, E>(graph, exitNode).analyse();
        for (Map.Entry<N, Set<N>> entry : postDominatorsOf.entrySet()) {
            LOGGER.log(Level.FINEST, "Post-dominators of {0} : {1}",
                    new Object[] {entry.getKey(), entry.getValue()});
        }

        // check that exit node post dominate every other nodes
        Set<N> notPostDominatedByExit = new HashSet<N>();
        for (Map.Entry<N, Set<N>> entry : postDominatorsOf.entrySet()) {
            if (!entry.getValue().contains(exitNode)) {
                notPostDominatedByExit.add(entry.getKey());
            }
        }
        if (notPostDominatedByExit.size() > 0) {
            LOGGER.log(Level.WARNING, "Exit node do not post-dominate every other nodes : {0}"
                    , notPostDominatedByExit);
        }

        // check that there is not post domination cycle (example : node A post
        // dominate node B and node B post dominate node A
        for (Map.Entry<N, Set<N>> entry : postDominatorsOf.entrySet()) {
            N node = entry.getKey();
            for (N node2 : entry.getValue()) {
                if (!node.equals(node2)) {
                    if (postDominatorsOf.get(node2).contains(node)) {
                        LOGGER.log(Level.WARNING,
                                "Detect post dominance cycle : {0} <-> {1}"
                                , new Object[] {node, node2});

                    }
                }
            }
        }

        computeImmediatePostDominators();

        // build post-dominators tree
        postDominatorsTree = Trees.newTree(exitNode);
        Multimap<N, N> children = HashMultimap.create();
        for (Map.Entry<N, N> entry : immediatePostDominator.entrySet()) {
            children.put(entry.getValue(), entry.getKey());
        }
        DominatorInfo.buildTree(exitNode, postDominatorsTree, children, factory);
        LOGGER.log(Level.FINEST, "Post dominators tree :\n{0}", Trees.toString(postDominatorsTree));

        // compute post dominance frontier
        computePostDominanceFrontier();
    }
}
