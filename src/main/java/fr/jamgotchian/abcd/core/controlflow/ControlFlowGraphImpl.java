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

package fr.jamgotchian.abcd.core.controlflow;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.common.LabelManager;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.graph.DirectedGraphs;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import fr.jamgotchian.abcd.core.graph.Matrix;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.graph.Trees;
import fr.jamgotchian.abcd.core.graph.EdgeFactory;
import fr.jamgotchian.abcd.core.util.Range;
import fr.jamgotchian.abcd.core.util.RangeImpl;
import fr.jamgotchian.abcd.core.util.RangeMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ControlFlowGraphImpl implements ControlFlowGraph {

    private static final Logger logger = Logger.getLogger(ControlFlowGraphImpl.class.getName());

    static {
        logger.setLevel(Level.FINEST);
    }

    private final String name;

    private InsnList instructions;

    private LabelManager labelManager;

    private final BasicBlock entryBlock;

    private final BasicBlock exitBlock;

    private final MutableDirectedGraph<BasicBlock, Edge> graph;

    private final RangeMap<Range, BasicBlock> basicBlocks;

    private final Map<BasicBlock, NaturalLoop> naturalLoops;

    private Tree<BasicBlock, Edge> dfst;

    private DominatorInfo<BasicBlock, Edge> dominatorInfo;

    private final Multimap<BasicBlock, ForkJoinInfo> joinBlocks;

    private final Map<BasicBlock, ForkJoinInfo> forkBlocks;

    private final List<ExceptionHandler> exceptionHandlers;

    private static class EdgeFactoryImpl implements EdgeFactory<Edge> {

        public Edge createEdge() {
            return new EdgeImpl();
        }
    } 
    
    public ControlFlowGraphImpl(String name, InsnList instructions) {
        this(name, new BasicBlockImpl(Integer.MIN_VALUE, -1, BasicBlockType.ENTRY));
        if (instructions == null) {
            throw new IllegalArgumentException("instructions == null");
        }
        this.instructions = instructions;
        labelManager = new LabelManager(instructions);
        BasicBlock instnBlock = new BasicBlockImpl(0, instructions.size()-1, null);
        addBasicBlock(instnBlock);
        addEdge(entryBlock, instnBlock);
    }

    public ControlFlowGraphImpl(String name, BasicBlock entryBlock) {
        if (name == null) {
            throw new IllegalArgumentException("name == null");
        }
        this.name = name;
        this.entryBlock = entryBlock;
        exitBlock = new BasicBlockImpl(BasicBlockType.EXIT);
        graph = DirectedGraphs.newDirectedGraph();
        basicBlocks = new RangeMap<Range, BasicBlock>();
        naturalLoops = new HashMap<BasicBlock, NaturalLoop>();
        joinBlocks = HashMultimap.create();
        forkBlocks = new HashMap<BasicBlock, ForkJoinInfo>();
        exceptionHandlers = new ArrayList<ExceptionHandler>();
        addBasicBlock(entryBlock);
        addBasicBlock(exitBlock);
    }

    public ControlFlowGraphImpl(String name) {
        this(name, new BasicBlockImpl(BasicBlockType.ENTRY));
    }

    public String getName() {
        return name;
    }

    public InsnList getInstructions() {
        return instructions;
    }

    public LabelManager getLabelManager() {
        return labelManager;
    }

    public DirectedGraph<BasicBlock, Edge> getGraph() {
        return DirectedGraphs.unmodifiableDirectedGraph(graph);
    }
    
    public BasicBlock getEntryBlock() {
        return entryBlock;
    }

    public BasicBlock getExitBlock() {
        return exitBlock;
    }

    public DominatorInfo<BasicBlock, Edge> getDominatorInfo() {
        return dominatorInfo;
    }

    public Multimap<BasicBlock, ForkJoinInfo> getJoinBlocks() {
        return Multimaps.unmodifiableMultimap(joinBlocks);
    }

    public Tree<BasicBlock, Edge> getDFST() {
        return dfst;
    }

    public void visit(ControlFlowGraphVisitor visitor) {
        assert dfst != null;
        visitor.visitBlock(this, entryBlock, null);
        for (Edge e : dfst.getEdges()) {
            visitor.visitBlock(this, graph.getEdgeTarget(e), e);
        }
    }

    public Map<BasicBlock, NaturalLoop> getNaturalLoops() {
        return naturalLoops;
    }

    public int getBasicBlockCount() {
        return graph.getVertices().size();
    }

    public Collection<BasicBlock> getBasicBlocks() {
        return graph.getVertices();
    }

    public BasicBlock getBasicBlock(Range range) {
        return basicBlocks.get(range);
    }

    public BasicBlock getBasicBlock(int first, int last) {
        return getBasicBlock(new RangeImpl(first, last));
    }

    public Collection<Edge> getOutgoingEdgesOf(BasicBlock block) {
        return graph.getOutgoingEdgesOf(block);
    }

    public Edge getFirstOutgoingEdgeOf(BasicBlock block) {
        return graph.getFirstOutgoingEdgeOf(block);
    }

    public Collection<Edge> getIncomingEdgesOf(BasicBlock block) {
        return graph.getIncomingEdgesOf(block);
    }

    public Edge getFirstIncomingEdgeOf(BasicBlock block) {
        return graph.getFirstIncomingEdgeOf(block);
    }

    public Collection<BasicBlock> getPredecessorsOf(BasicBlock block) {
        return graph.getPredecessorsOf(block);
    }

    public int getPredecessorCountOf(BasicBlock block) {
        return graph.getPredecessorCountOf(block);
    }

    public BasicBlock getFirstPredecessorOf(BasicBlock block) {
        return graph.getFirstPredecessorOf(block);
    }

    public Collection<BasicBlock> getSuccessorsOf(BasicBlock block) {
        return graph.getSuccessorsOf(block);
    }

    public BasicBlock getFirstSuccessorOf(BasicBlock block) {
        return graph.getFirstSuccessorOf(block);
    }

    public int getSuccessorCountOf(BasicBlock block) {
        return graph.getSuccessorCountOf(block);
    }

    public Collection<BasicBlock> getBasicBlocksWithinRange(Range range) {
        return basicBlocks.values(range);
    }

    public Collection<BasicBlock> getBasicBlocksWithinRange(int first, int last) {
        return getBasicBlocksWithinRange(new RangeImpl(first, last));
    }

    public boolean isBasicBlockReachable(BasicBlock block) {
        return graph.getIncomingEdgesOf(block).size() > 0 || graph.getOutgoingEdgesOf(block).size() > 0;
    }

    public Collection<BasicBlock> getNonEmptyBasicBlocks() {
        return basicBlocks.values();
    }

    final public void addBasicBlock(BasicBlock block) {
        block.setGraph(this);
        graph.addVertex(block);
        if (block.getRange() != null) {
            basicBlocks.put(block.getRange(), block);
        }
    }

    public BasicBlockSplit splitBasicBlockAt(int index) {
        if (index == Integer.MIN_VALUE) {
            throw new ABCDException("Can't split at index Integer.MIN_VALUE");
        }
        BasicBlock blockAfter = basicBlocks.findBeginningAt(index);
        BasicBlock blockBefore = basicBlocks.findContaining(index - 1);
        Edge edge = null;
        if (blockAfter == null) {
            if (blockBefore == null) {
                throw new ABCDException("Split index (" + index + ") out of range");
            }

            if (index <= blockBefore.getRange().getLast()) {
                logger.log(Level.FINEST, "  Split {0} at {1}", new Object[]{blockBefore, index});

                int last = blockBefore.getRange().getLast();

                // resize the block before the split
                resizeBasicBlock(blockBefore, index - 1);

                // create the block after the split
                blockAfter = new BasicBlockImpl(index, last, blockBefore.getType());
                blockAfter.setGraph(this);
                graph.splitVertex(blockBefore, blockAfter);
                basicBlocks.put(blockAfter.getRange(), blockAfter);

                blockBefore.setType(null);

                edge = addEdge(blockBefore, blockAfter);
            }
        } else {
          edge = graph.getEdge(blockBefore, blockAfter);
        }

        return new BasicBlockSplit(blockBefore, blockAfter, edge);
    }

    private void resizeBasicBlock(BasicBlock block, int newLast) {
        if (block != null) {
            basicBlocks.remove(block.getRange());
            block.getRange().setLast(newLast);
            basicBlocks.put(block.getRange(), block);
        }
    }

    final public Edge addEdge(BasicBlock source, BasicBlock target) {
        return addEdge(source, target, false);
    }

    public Edge addEdge(BasicBlock source, BasicBlock target, boolean exceptional) {
        if (source == null) {
            throw new IllegalArgumentException("source == null");
        }
        if (target == null) {
            throw new IllegalArgumentException("target == null");
        }

        Edge edge = graph.getEdge(source, target);
        if (edge == null) {
            logger.log(Level.FINEST, "  Create edge between {0} and {1}", new Object[]{source, target});

            edge = new EdgeImpl(exceptional);
            graph.addEdge(source, target, edge);
        }

        return edge;
    }

    public boolean removeEdge(BasicBlock source, BasicBlock target) {
        logger.log(Level.FINEST, "  Remove edge between {0} and {1}", new Object[]{source, target});

        return graph.removeEdge(source, target);
    }

    public Edge getEdge(BasicBlock source, BasicBlock target) {
        return graph.getEdge(source, target);
    }

    public Set<Edge> getEdges() {
        return graph.getEdges();
    }

    public boolean containsEdge(BasicBlock source, BasicBlock target) {
        return graph.containsEdge(source, target);
    }

    public BasicBlock getEdgeSource(Edge edge) {
        return graph.getEdgeSource(edge);
    }

    public BasicBlock getEdgeTarget(Edge edge) {
        return graph.getEdgeTarget(edge);
    }

    public void analyse() {
        removeUnreachableBlock();
        removeUnnecessaryBlock();
        dominatorInfo = DominatorInfo.create(graph, entryBlock, exitBlock, new EdgeFactoryImpl());
        performDepthFirstSearch();
        analyseForkJoin();
        analyseEdgeCategory();
        analyseNaturalLoops();
        analyseLoopLevel();
        analyseExceptionHandlers();
    }

    private void performDepthFirstSearch() {
        // build depth first spanning Tree
        dfst = graph.getReversePostOrderDFST(entryBlock, false);
        int order = 0;
        for (BasicBlock block : dfst.getNodes()) {
            block.setOrder(order++);
        }

        logger.log(Level.FINEST, "Perform reverse post order DFS");
        logger.log(Level.FINEST, "DFST : \n{0}", Trees.toString(dfst));
    }

    private void removeUnreachableBlock() {
        for (BasicBlock block : new HashSet<BasicBlock>(graph.getVertices())) {
            if (!block.equals(entryBlock) && !block.equals(exitBlock) 
                    && graph.getIncomingEdgesOf(block).isEmpty() 
                    && graph.getOutgoingEdgesOf(block).isEmpty()) {
                graph.removeVertex(block);

                logger.log(Level.FINER, "Remove unreachable block {0}", block);
            }
        }
    }

    private void removeUnnecessaryBlock() {
        Set<BasicBlock> blocksToRemove = new HashSet<BasicBlock>();
        for (BasicBlock block : graph.getVertices()) {
            if (graph.getIncomingEdgesOf(block).size() == 1 &&
                graph.getOutgoingEdgesOf(block).size() == 1) {
                Range range = block.getRange();
                if (range != null && range.size() > 0) {
                    AbstractInsnNode node = block.getGraph().getInstructions().get(range.getLast());
                    if (node.getType() == AbstractInsnNode.JUMP_INSN &&
                        node.getOpcode() == Opcodes.GOTO) {
                        boolean remove = true;
                        for (int i = range.getFirst(); i <= range.getLast()-1; i++) {
                            node = block.getGraph().getInstructions().get(i);
                            if (node.getType() != AbstractInsnNode.FRAME &&
                                node.getType() != AbstractInsnNode.LABEL &&
                                node.getType() != AbstractInsnNode.LINE) {
                                remove = false;
                                break;
                            }
                        }
                        if (remove) {
                            blocksToRemove.add(block);
                        }
                    }
                }
            }
        }

        for (BasicBlock block : blocksToRemove) {
            Edge incomingEdge = graph.getIncomingEdgesOf(block).iterator().next();
            Edge outgoingEdge = graph.getOutgoingEdgesOf(block).iterator().next();
            BasicBlock predecessor = graph.getEdgeSource(incomingEdge);
            BasicBlock successor = graph.getEdgeTarget(outgoingEdge);
            graph.removeEdge(incomingEdge);
            graph.removeEdge(outgoingEdge);
            graph.removeVertex(block);
            graph.addEdge(predecessor, successor, incomingEdge);

            logger.log(Level.FINER, "Remove unnecessary block {0}", block);
        }
    }

    private void analyseForkJoin() {
        // update fork / join infos
        joinBlocks.clear();
        forkBlocks.clear();
        for (BasicBlock forkBlock : dfst.getNodes()) {
            if (graph.getSuccessorsOf(forkBlock).size() > 1) {
                BasicBlock joinBlock = dominatorInfo.getPostDominatorsTree().getParent(forkBlock);
                Set<ForkJoinInfo.Branch> branches = new HashSet<ForkJoinInfo.Branch>();
                for (Edge forkEdge : graph.getOutgoingEdgesOf(forkBlock)) {
                    BasicBlock target = graph.getEdgeTarget(forkEdge);
                    if (target.equals(joinBlock)) {
                        branches.add(new ForkJoinInfo.Branch(forkEdge, forkEdge));
                    } else {
                        Edge joinEdge = dominatorInfo.getDominanceFrontierOf(target).iterator().next();
                        branches.add(new ForkJoinInfo.Branch(forkEdge, joinEdge));
                    }
                }
                ForkJoinInfo info = new ForkJoinInfo(forkBlock, joinBlock, branches);
                joinBlocks.put(joinBlock, info);
                forkBlocks.put(forkBlock, info);
                logger.log(Level.FINEST, "Fork at {0}, join at {1}",
                            new Object[]{forkBlock, joinBlock});
                for (ForkJoinInfo.Branch branch : branches) {
                    logger.log(Level.FINEST, "  Branch start at {0}, end at {1}",
                                new Object[]{graph.toString(branch.getForkEdge()),
                                             graph.toString(branch.getJoinEdge())});
                }
            }
        }
    }

    public Matrix<Boolean> calculateAncestorsMatrix(Collection<Edge> edges) {
        int nodeCount = graph.getVertices().size();
        Matrix<Boolean> ancestorsMatrix = new Matrix<Boolean>(nodeCount, nodeCount, Boolean.FALSE);

        for (Edge edge : edges) {
            BasicBlock source = graph.getEdgeSource(edge);
            BasicBlock target = graph.getEdgeTarget(edge);
            ancestorsMatrix.setValue(source.getOrder(), target.getOrder(), Boolean.TRUE);
        }

        // compute the transitive closure of the ancestor matrix
        for(int i = 0; i < nodeCount; i++) {
            for(int j = 0; j < nodeCount; j++) {
                if(Boolean.TRUE.equals(ancestorsMatrix.getValue(i, j))) {
                    for(int k = 0; k < nodeCount; k++) {
                        if(Boolean.TRUE.equals(ancestorsMatrix.getValue(j, k))) {
                            ancestorsMatrix.setValue(i, k, Boolean.TRUE);
                        }
                    }
                }
            }
        }

        return ancestorsMatrix;
    }

    private void analyseEdgeCategory() {
        Matrix<Boolean> ancestorsMatrix = calculateAncestorsMatrix(dfst.getEdges());

        for (Edge e : graph.getEdges()) {
            BasicBlock source = graph.getEdgeSource(e);
            BasicBlock target = graph.getEdgeTarget(e);
            if (ancestorsMatrix.getValue(source.getOrder(), target.getOrder()) ||
                ancestorsMatrix.getValue(target.getOrder(), source.getOrder())) {
                if (target.getOrder() > source.getOrder()) {
                    e.setCategory(EdgeCategory.ADVANCING);
                } else if (target.getOrder() < source.getOrder()) {
                    if (dominatorInfo.getDominatorsOf(source).contains(target)) {
                        e.setCategory(EdgeCategory.BACK);
                    } else {
                        e.setCategory(EdgeCategory.RETREATING);
                    }
                }
            } else {
                e.setCategory(EdgeCategory.CROSS);
            }
            
            // self loop
            if (source.equals(target)) {
                e.setSelfLoop(true);
            }

            logger.log(Level.FINEST, "Edge Category of {0} -> {1} : {2}",
                    new Object[] {source, target, e.getCategory()});
        }
    }

    private void analyseNaturalLoops() {
        naturalLoops.clear();
        for (Edge e : graph.getEdges()) {
            switch (e.getCategory()) {
                case BACK: {
                    BasicBlock head = graph.getEdgeTarget(e);
                    BasicBlock v = graph.getEdgeSource(e);
                    Set<BasicBlock> visited = new HashSet<BasicBlock>();
                    visited.add(head);
                    List<BasicBlock> loop = new ArrayList<BasicBlock>();
                    loop.add(head);
                    graph.reversePostOrderDFS(v, visited, loop, null, true);
                    NaturalLoop nl = new NaturalLoop(head, loop);
                    naturalLoops.put(head, nl);

                    logger.log(Level.FINER, " Found natural loop : {0}", nl);
                    break;
                }

                case RETREATING: {
                    throw new ABCDException("Irreducible control flow detected");
                }
            }
        }
    }

    private void analyseLoopLevel() {
        // from outermost to innermost loops
        for (BasicBlock loopHead : dfst.getNodes()) {
            NaturalLoop nl = naturalLoops.get(loopHead);
            if (nl != null) {
                // increase loop level for all blocks of the loop
                for (BasicBlock block : nl.getLoop()) {
                    block.setLoopLevel(block.getLoopLevel()+1);
                }
            }
        }
            
        // self loop
        for (Edge edge : graph.getEdges()) {
            if (edge.isSelfLoop()) {
                BasicBlock block = graph.getEdgeSource(edge);
                block.setLoopLevel(block.getLoopLevel()+1);
            }
        }

        for (BasicBlock block : dfst.getNodes()) {
            logger.log(Level.FINEST, "Loop level of {0} : {1}",
                    new Object[] {block, block.getLoopLevel()});
        }

        for (Edge edge : graph.getEdges()) {
            BasicBlock source = graph.getEdgeSource(edge);
            BasicBlock target = graph.getEdgeTarget(edge);
            if (target.getLoopLevel() < source.getLoopLevel()) {
                logger.log(Level.FINEST, "Loop exit edge {0}", graph.toString(edge));
                edge.setLoopExit(true);
            }
       }
    }

    private void analyseExceptionHandlers() {
        exceptionHandlers.clear();
        Set<BasicBlock> handlerEntries = new HashSet<BasicBlock>();
        for (Edge edge : graph.getEdges()) {
            if (edge.isExceptional()) {
                handlerEntries.add(graph.getEdgeTarget(edge));
            }
        }
        for (BasicBlock handlerEntry : handlerEntries) {
            Set<BasicBlock> handledBlocks = new HashSet<BasicBlock>(graph.getPredecessorsOf(handlerEntry));
            exceptionHandlers.add(new ExceptionHandler(handlerEntry, handledBlocks));
            logger.log(Level.FINEST, "Exception handler : entryBlock={0}, handledBlocks={1}",
                    new Object[] {handlerEntry, handledBlocks});
        }
    }

    public String toString(Collection<Edge> edges) {
        return graph.toString(edges);
    }

    public String toString(Edge edge) {
        return graph.toString(edge);
    }

    @Override
    public String toString() {
        return name;
    }
}
