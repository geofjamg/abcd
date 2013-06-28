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

import com.google.common.collect.Sets;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DominatorInfo<N, E> {

    private static final Logger LOGGER = LoggerFactory.getLogger(DominatorInfo.class.getName());

    private final DirectedGraph<N, E> graph;

    private final N entryNode;

    private final EdgeFactory<E> factory;

    private Map<N, Set<N>> dominatorsOf;

    private Map<N, N> immediateDominator;

    private MutableTree<N, E> dominatorsTree;

    private Map<N, Set<E>> dominanceFrontierOf;

    public static <N, E> DominatorInfo<N, E>
            create(DirectedGraph<N, E> graph, N entryNode, EdgeFactory<E> factory) {
        DominatorInfo<N, E> domInfo = new DominatorInfo<>(graph, entryNode, factory);
        domInfo.update();
        return domInfo;
    }

    private DominatorInfo(DirectedGraph<N, E> graph, N entryNode, EdgeFactory<E> factory) {
        this.graph = graph;
        this.entryNode = entryNode;
        this.factory = factory;
    }

    public Set<N> getDominatorsOf(N n) {
        return Collections.unmodifiableSet(dominatorsOf.get(n));
    }

    public boolean dominates(N x, N y) {
        return dominatorsOf.get(y).contains(x);
    }

    public boolean strictlyDominates(N x, N y) {
        return !x.equals(y) && dominates(x, y);
    }

    public N getImmediateDominatorOf(N n) {
        return immediateDominator.get(n);
    }

    public Tree<N, E> getDominatorsTree() {
        return Trees.unmodifiableTree(dominatorsTree);
    }

    public Set<E> getDominanceFrontierOf(N n) {
        return Collections.unmodifiableSet(dominanceFrontierOf.get(n));
    }

    public Set<N> getDominanceFrontierOf2(N n) {
        Set<N> frontier = new HashSet<>();
        for (E e : dominanceFrontierOf.get(n)) {
            frontier.add(graph.getEdgeTarget(e));
        }
        return frontier;
    }

    private void computeImmediateDominators() {
        // the immediate dominator of a node n is obtained by choosing the strict
        // dominator of n whose dominator set differs from that of n only by removal
        // of n.
        immediateDominator = new HashMap<>();
        for (Map.Entry<N, Set<N>> entry : dominatorsOf.entrySet()) {
            N n = entry.getKey();
            if (!n.equals(entryNode)) {
                Set<N> dominators = entry.getValue();
                boolean found = false;
                for (N d : dominators) {
                    if (!d.equals(n)
                            && Objects.equals(Sets.union(dominatorsOf.get(d),
                                                         Collections.singleton(n)),
                                              dominators)) {
                        immediateDominator.put(n, d);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    LOGGER.warn("Cannot find immediate dominator of {}",  n);
                }
            }
        }
        LOGGER.trace("Immediate dominators {}", immediateDominator);
    }

    private void computeDominanceFrontier() {
        dominanceFrontierOf = new HashMap<>();

        // x dominate a predecessor of z (y) but do not strictly dominate z
        for (N x : graph.getVertices()) {
            dominanceFrontierOf.put(x, new HashSet<E>());
            if (dominatorsTree.containsNode(x)) {
                for (N y : dominatorsTree.getSubTree(x).getNodes()) {
                    for (N z : graph.getSuccessorsOf(y)) {
                        if (!z.equals(x)) {
                            if (!strictlyDominates(x, z)) {
                                E yz = graph.getEdge(y, z);
                                dominanceFrontierOf.get(x).add(yz);
                            }
                        }
                    }
                }
            }

            LOGGER.trace("Dominance frontier of {} : {}",
                    x, graph.toString(dominanceFrontierOf.get(x)));
       }
    }

    public void update() {
        LOGGER.debug("Update dominator info");

        // find dominators
        dominatorsOf = new DominatorsFinder<>(graph, entryNode).analyse();
        for (Map.Entry<N, Set<N>> entry : dominatorsOf.entrySet()) {
            LOGGER.trace("Dominators of {} : {}",
                    entry.getKey(), entry.getValue());
        }

        computeImmediateDominators();

        // build dominators tree
        dominatorsTree = Trees.newTree(entryNode, immediateDominator, factory);
        LOGGER.trace("Dominators tree :\n{}", Trees.toString(dominatorsTree));

        // compute dominance frontier
        computeDominanceFrontier();
    }
}
