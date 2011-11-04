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
public class DominatorInfo<N, E> {

    private static final Logger logger = Logger.getLogger(DominatorInfo.class.getName());

    private final DirectedGraph<N, E> graph;

    private final N entryNode;

    private final EdgeFactory<E> factory;

    private Map<N, Set<N>> dominatorsOf;

    private Map<N, N> immediateDominator;

    private MutableTree<N, E> dominatorsTree;

    private Map<N, Set<E>> dominanceFrontierOf;

    public static <N, E> DominatorInfo<N, E>
            create(DirectedGraph<N, E> graph, N entryNode, EdgeFactory<E> factory) {
        DominatorInfo<N, E> domInfo = new DominatorInfo<N, E>(graph, entryNode, factory);
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
        Set<N> frontier = new HashSet<N>();
        for (E e : dominanceFrontierOf.get(n)) {
            frontier.add(graph.getEdgeTarget(e));
        }
        return frontier;
    }

    private void computeImmediateDominators() {
        // the immediate dominator of a node n is obtained by choosing the strict
        // dominator of n whose dominator set differs from that of n only by removal
        // of n.
        immediateDominator = new HashMap<N, N>();
        for (Map.Entry<N, Set<N>> entry : dominatorsOf.entrySet()) {
            N n = entry.getKey();
            if (!n.equals(entryNode)) {
                Set<N> dominators = entry.getValue();
                boolean found = false;
                for (N d : dominators) {
                    if (!d.equals(n)
                            && Collections3.sameContent(Sets.union(dominatorsOf.get(d),
                                                                   Collections.singleton(n)),
                                                        dominators)) {
                        immediateDominator.put(n, d);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    logger.log(Level.WARNING, "Cannot find immediate dominator of {0}",  n);
                }
            }
        }
        logger.log(Level.FINEST, "Immediate dominators {0}", immediateDominator);
    }

    private void computeDominanceFrontier() {
        dominanceFrontierOf = new HashMap<N, Set<E>>();

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

            logger.log(Level.FINEST, "Dominance frontier of {0} : {1}",
                    new Object[] {x, graph.toString(dominanceFrontierOf.get(x))});
       }
    }

    static <N, E> void buildTree(N parent, MutableTree<N, E> tree,
                                 Multimap<N, N> children, EdgeFactory<E> factory) {
        for (N child : children.get(parent)) {
            tree.addNode(parent, child, factory.createEdge());
            buildTree(child, tree, children, factory);
        }
    }

    public void update() {
        logger.log(Level.FINER, "Update dominator info");

        // find dominators
        dominatorsOf = new DominatorsFinder<N, E>(graph, entryNode).analyse();
        for (Map.Entry<N, Set<N>> entry : dominatorsOf.entrySet()) {
            logger.log(Level.FINEST, "Dominators of {0} : {1}",
                    new Object[] {entry.getKey(), entry.getValue()});
        }

        computeImmediateDominators();

        // build dominators tree
        dominatorsTree = Trees.newTree(entryNode);
        Multimap<N, N> children = HashMultimap.create();
        for (Map.Entry<N, N> entry : immediateDominator.entrySet()) {
            children.put(entry.getValue(), entry.getKey());
        }
        buildTree(entryNode, dominatorsTree, children, factory);
        logger.log(Level.FINEST, "Dominators tree :\n{0}", Trees.toString(dominatorsTree));

        // compute dominance frontier
        computeDominanceFrontier();
    }
}
