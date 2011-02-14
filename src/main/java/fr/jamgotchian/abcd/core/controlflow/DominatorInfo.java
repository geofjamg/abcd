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

package fr.jamgotchian.abcd.core.controlflow;

import fr.jamgotchian.abcd.core.graph.MutableTree;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.graph.Trees;
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
public class DominatorInfo {

    private static final Logger logger = Logger.getLogger(DominatorInfo.class.getName());

    static {
        logger.setLevel(Level.FINE);
    }
    
    private final ControlFlowGraph graph;

    private Map<BasicBlock, Set<BasicBlock>> dominatorsOf;

    private MutableTree<BasicBlock, Edge> dominatorsTree;

    private Map<BasicBlock, Set<Edge>> dominanceFrontierOf;

    private Map<BasicBlock, Set<BasicBlock>> postDominatorsOf;

    private MutableTree<BasicBlock, Edge> postDominatorsTree;

    private Map<BasicBlock, Set<Edge>> postDominanceFrontierOf;

    public static DominatorInfo create(ControlFlowGraph graph) {
        DominatorInfo domInfo = new DominatorInfo(graph);
        domInfo.update();
        return domInfo;
    }

    private DominatorInfo(ControlFlowGraph graph) {
        this.graph = graph;
    }

    public Set<BasicBlock> getDominatorsOf(BasicBlock b) {
        return dominatorsOf.get(b);
    }
    
    public boolean dominate(BasicBlock x, BasicBlock y) {
        return dominatorsOf.get(y).contains(x);
    }

    public boolean strictlyDominate(BasicBlock x, BasicBlock y) {
        return !x.equals(y) && dominate(x, y);
    }
    
    public BasicBlock getImmediateDominatorOf(BasicBlock b) {
        return dominatorsTree.getParent(b);
    }

    public Tree<BasicBlock, Edge> getDominatorsTree() {
        return dominatorsTree;
    }

    public Set<Edge> getDominanceFrontierOf(BasicBlock b) {
        return dominanceFrontierOf.get(b);
    }

    public Set<BasicBlock> getPostDominatorsOf(BasicBlock b) {
        return postDominatorsOf.get(b);
    }

    public Tree<BasicBlock, Edge> getPostDominatorsTree() {
        return postDominatorsTree;
    }

    public Set<Edge> getPostDominanceFrontierOf(BasicBlock b) {
        return postDominanceFrontierOf.get(b);
    }

    public boolean postDominate(BasicBlock x, BasicBlock y) {
        return postDominatorsOf.get(y).contains(x);
    }

    public boolean strictlyPostDominate(BasicBlock x, BasicBlock y) {
        return !x.equals(y) && postDominate(x, y);
    }
    
    public BasicBlock getImmediatePostDominatorOf(BasicBlock b) {
        return postDominatorsTree.getParent(b);
    }

    private void buildTree(Map<BasicBlock, Set<BasicBlock>> dominatorsOf,
                           BasicBlock parentBlock, MutableTree<BasicBlock, Edge> tree) {
        for (Map.Entry<BasicBlock, Set<BasicBlock>> entry : dominatorsOf.entrySet()) {
            BasicBlock block = entry.getKey();
            if (!block.equals(parentBlock)) {
                Set<BasicBlock> dominators = entry.getValue();
                if (dominators.contains(parentBlock)) {
                    if (tree.containsNode(block)) {
                        tree.setParent(block, parentBlock);
                    } else {
                        tree.addNode(parentBlock, block, new EdgeImpl());
                    }
                }
            }
        }
        for (BasicBlock child : new HashSet<BasicBlock>(tree.getChildren(parentBlock))) {
            buildTree(dominatorsOf, child, tree);
        }
    }

    public void computeDominanceFrontier() {
        dominanceFrontierOf = new HashMap<BasicBlock, Set<Edge>>();

        for (BasicBlock x : graph.getBasicBlocks()) {
            dominanceFrontierOf.put(x, new HashSet<Edge>());
            for (BasicBlock y : dominatorsTree.getSubTree(x).getNodes()) {
                for (BasicBlock z : graph.getSuccessorsOf(y)) {
                    if (!strictlyDominate(x, z)) { 
                        Edge yz = graph.getEdge(y, z);
                        dominanceFrontierOf.get(x).add(yz);
                    }
                }
            }
            
            logger.log(Level.FINEST, "Dominance frontier of {0} : {1}",
                    new Object[] {x, graph.toString(dominanceFrontierOf.get(x))});
       }
    }
    
    public void computePostDominanceFrontier() {
        postDominanceFrontierOf = new HashMap<BasicBlock, Set<Edge>>();

        for (BasicBlock x : graph.getBasicBlocks()) {
            postDominanceFrontierOf.put(x, new HashSet<Edge>());
            for (BasicBlock y : postDominatorsTree.getSubTree(x).getNodes()) {
                for (BasicBlock z : graph.getPredecessorsOf(y)) {
                    if (!strictlyPostDominate(x, z)) { 
                        Edge zy = graph.getEdge(z, y);
                        postDominanceFrontierOf.get(x).add(zy);
                    }
                }
            }
            
            logger.log(Level.FINEST, "Post dominance frontier of {0} : {1}",
                    new Object[] {x, graph.toString(postDominanceFrontierOf.get(x))});
       }
    }
    
    public void update() {
        logger.log(Level.FINER, "Update dominator info");
            
        // find dominators
        dominatorsOf = new DominatorsFinder(graph).analyse();
        for (Map.Entry<BasicBlock, Set<BasicBlock>> entry : dominatorsOf.entrySet()) {
            logger.log(Level.FINEST, "Dominators of {0} : {1}",
                    new Object[] {entry.getKey(), entry.getValue()});
        }

        // build dominators tree
        dominatorsTree = Trees.newTree(graph.getEntryBlock());
        buildTree(dominatorsOf, graph.getEntryBlock(), dominatorsTree);
        logger.log(Level.FINEST, "Dominators tree :\n{0}", Trees.toString(dominatorsTree));
        
        // compute dominance frontier
        computeDominanceFrontier();

        // find post-dominators
        postDominatorsOf = new PostDominatorsFinder(graph).analyse();
        for (Map.Entry<BasicBlock, Set<BasicBlock>> entry : postDominatorsOf.entrySet()) {
            logger.log(Level.FINEST, "Post-dominators of {0} : {1}",
                    new Object[] {entry.getKey(), entry.getValue()});
        }

        // build post-dominators tree
        postDominatorsTree = Trees.newTree(graph.getExitBlock());
        buildTree(postDominatorsOf, graph.getExitBlock(), postDominatorsTree);
        logger.log(Level.FINEST, "Post dominators tree :\n{0}", Trees.toString(postDominatorsTree));
        
        // compute post dominance frontier
        computePostDominanceFrontier();
    }
}
