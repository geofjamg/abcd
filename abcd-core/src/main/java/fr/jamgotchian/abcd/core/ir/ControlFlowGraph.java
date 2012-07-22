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

package fr.jamgotchian.abcd.core.ir;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.graph.PostDominatorInfo;
import fr.jamgotchian.abcd.core.graph.DominatorInfo;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.graph.DirectedGraphs;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
import fr.jamgotchian.abcd.core.graph.Matrix;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.graph.Trees;
import fr.jamgotchian.abcd.core.graph.EdgeFactory;
import fr.jamgotchian.abcd.core.graph.VertexFactory;
import fr.jamgotchian.abcd.core.util.Range;
import fr.jamgotchian.abcd.core.util.RangeImpl;
import fr.jamgotchian.abcd.core.util.RangeMap;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ControlFlowGraph {

    private static final Logger LOGGER = LoggerFactory.getLogger(ControlFlowGraph.class);

    private static final IRGraphvizRenderer IR_GRAPHVIZ_RENDERER
            = new IRGraphvizRenderer();

    private static final EdgeGraphvizRenderer EDGE_GRAPHVIZ_RENDERER
            = new EdgeGraphvizRenderer();

    private static final RangeGraphvizRenderer RANGE_GRAPHVIZ_RENDERER
            = new RangeGraphvizRenderer();

    private String name;

    private final BasicBlock entryBlock;

    private BasicBlock exitBlock;

    private final MutableDirectedGraph<BasicBlock, Edge> graph;

    private final RangeMap<Range, BasicBlock> basicBlocks;

    private final Multimap<BasicBlock, NaturalLoop> naturalLoops;

    private List<NaturalLoop> outermostLoops;

    private Tree<BasicBlock, Edge> dfst;

    private DominatorInfo<BasicBlock, Edge> dominatorInfo;

    private PostDominatorInfo<BasicBlock, Edge> postDominatorInfo;

    private LocalVariableTable localVariableTable;

    private ExceptionTable exceptionTable;

    private static final EdgeFactory<Edge> EDGE_FACTORY = new EdgeFactoryImpl();

    private static final VertexFactory<BasicBlock> VIRTUAL_EXIT_FACTORY
            = new VirtualExitBasicBlockFactory();

    public ControlFlowGraph(String name, int instructionCount) {
        this(name, BasicBlockImpl.createEntry(),  BasicBlockImpl.createExit());
        if (instructionCount > 0 ) {
            BasicBlock instnBlock = BasicBlockImpl.createRange(0, instructionCount-1, null);
            addBasicBlock(instnBlock);
            addEdge(entryBlock, instnBlock);
        } else {
            addEdge(entryBlock, exitBlock);
        }
    }

    public ControlFlowGraph(String name, BasicBlock entryBlock) {
        this(name, entryBlock, BasicBlockImpl.createExit());
    }

    public ControlFlowGraph(String name, BasicBlock entryBlock, BasicBlock exitBlock) {
        if (name == null) {
            throw new IllegalArgumentException("name == null");
        }
        this.name = name;
        this.entryBlock = entryBlock;
        this.exitBlock = exitBlock;
        graph = DirectedGraphs.newDirectedGraph();
        basicBlocks = new RangeMap<Range, BasicBlock>();
        naturalLoops = HashMultimap.create();
        outermostLoops = new ArrayList<NaturalLoop>();
        addBasicBlock(entryBlock);
        addBasicBlock(exitBlock);
    }

    public ControlFlowGraph(String name) {
        this(name, BasicBlockImpl.createEntry(), BasicBlockImpl.createExit());
    }

    public ControlFlowGraph(ControlFlowGraph other) {
        this(other.getName(), other.entryBlock, other.exitBlock);
        for (BasicBlock bb : other.getBasicBlocks()) {
            if (!bb.equals(entryBlock) && !bb.equals(exitBlock)) {
                addBasicBlock(bb);
            }
        }
        for (Edge e : other.getEdges()) {
            addEdge(other.getEdgeSource(e), other.getEdgeTarget(e), e);
        }
        exceptionTable = other.exceptionTable;
        localVariableTable = other.localVariableTable;
    }

    public String getName() {
        return name;
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

    public void setExitBlock(BasicBlock exitBlock) {
        this.exitBlock = exitBlock;
    }

    public void updateDominatorInfo() {
        dominatorInfo = DominatorInfo.create(graph, entryBlock, EDGE_FACTORY);
    }

    public void updatePostDominatorInfo() {
        postDominatorInfo = PostDominatorInfo.create(graph, exitBlock, EDGE_FACTORY, VIRTUAL_EXIT_FACTORY);
    }

    public DominatorInfo<BasicBlock, Edge> getDominatorInfo() {
        return dominatorInfo;
    }

    public PostDominatorInfo<BasicBlock, Edge> getPostDominatorInfo() {
        return postDominatorInfo;
    }

    public Tree<BasicBlock, Edge> getDFST() {
        return dfst;
    }

    public Multimap<BasicBlock, NaturalLoop> getNaturalLoops() {
        return naturalLoops;
    }

    public Collection<NaturalLoop> getOutermostLoops() {
        return outermostLoops;
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

    public boolean containsBasicBlock(BasicBlock bb) {
        return graph.containsVertex(bb);
    }

    public Collection<BasicBlock> getBasicBlocksWithinRange(Range range) {
        return basicBlocks.values(range);
    }

    public Collection<BasicBlock> getBasicBlocksWithinRange(int first, int last) {
        return getBasicBlocksWithinRange(new RangeImpl(first, last));
    }

    public boolean isBasicBlockReachable(BasicBlock block) {
        return graph.getIncomingEdgesOf(block).size() > 0
                || graph.getOutgoingEdgesOf(block).size() > 0;
    }

    public Collection<BasicBlock> getNonEmptyBasicBlocks() {
        return basicBlocks.values();
    }

    public void addBasicBlock(BasicBlock block) {
        graph.addVertex(block);
        if (block.getRange() != null) {
            basicBlocks.put(block.getRange(), block);
        }
    }

    public void removeBasicBlock(BasicBlock block) {
        graph.removeVertex(block);
        if (block.getRange() != null) {
            basicBlocks.remove(block.getRange());
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
                LOGGER.trace("  Split {} at {}", blockBefore, index);

                int last = blockBefore.getRange().getLast();

                // resize the block before the split
                resizeBasicBlock(blockBefore, index - 1);

                // create the block after the split
                blockAfter = BasicBlockImpl.createRange(index, last, blockBefore.getType());
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

    public Edge addEdge(BasicBlock source, BasicBlock target) {
        return addEdge(source, target, false);
    }

    public void addEdge(BasicBlock source, BasicBlock target, Edge edge) {
        LOGGER.trace("  Create edge between {} and {}", source, target);

        graph.addEdge(source, target, edge);
    }

    public Edge addEdge(BasicBlock source, BasicBlock target, boolean exceptional) {
        Edge edge = graph.getEdge(source, target);
        if (edge == null) {
            LOGGER.trace("  Create edge between {} and {}", source, target);
            edge = EDGE_FACTORY.createEdge();
            if (exceptional) {
                edge.addAttribute(EdgeAttribute.EXCEPTIONAL_EDGE);
            }
            graph.addEdge(source, target, edge);
        }
        return edge;
    }

    public void removeEdge(Edge edge) {
        LOGGER.trace("  Remove edge between {} and {}",
                graph.getEdgeSource(edge), graph.getEdgeTarget(edge));

        graph.removeEdge(edge);
    }

    public boolean removeEdge(BasicBlock source, BasicBlock target) {
        LOGGER.trace("  Remove edge between {} and {}", source, target);

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

    public Collection<Edge> getOutgoingEdgesOf(BasicBlock block) {
        return graph.getOutgoingEdgesOf(block);
    }

    public Collection<Edge> getNormalOutgoingEdgesOf(BasicBlock block) {
        List<Edge> edges = new ArrayList<Edge>();
        for (Edge e : getOutgoingEdgesOf(block)) {
            if (!e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                edges.add(e);
            }
        }
        return edges;
    }

    public Collection<Edge> getExceptionalOutgoingEdgesOf(BasicBlock block) {
        List<Edge> edges = new ArrayList<Edge>();
        for (Edge e : getOutgoingEdgesOf(block)) {
            if (e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                edges.add(e);
            }
        }
        return edges;
    }

    public Edge getFirstOutgoingEdgeOf(BasicBlock block) {
        return graph.getFirstOutgoingEdgeOf(block);
    }

    public Edge getFirstNormalOutgoingEdgeOf(BasicBlock block) {
        for (Edge e : getOutgoingEdgesOf(block)) {
            if (!e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                return e;
            }
        }
        return null;
    }

    public Edge getFirstExceptionalOutgoingEdgeOf(BasicBlock block) {
        for (Edge e : getOutgoingEdgesOf(block)) {
            if (e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                return e;
            }
        }
        return null;
    }

    public Collection<Edge> getIncomingEdgesOf(BasicBlock block) {
        return graph.getIncomingEdgesOf(block);
    }

    public Edge getFirstIncomingEdgeOf(BasicBlock block) {
        return graph.getFirstIncomingEdgeOf(block);
    }

    public Edge getFirstNormalIncomingEdgeOf(BasicBlock block) {
        for (Edge e : getIncomingEdgesOf(block)) {
            if (!e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                return e;
            }
        }
        return null;
    }

    public Edge getFirstExceptionalIncomingEdgeOf(BasicBlock block) {
        for (Edge e : getIncomingEdgesOf(block)) {
            if (e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                return e;
            }
        }
        return null;
    }

    public Collection<BasicBlock> getPredecessorsOf(BasicBlock block) {
        return graph.getPredecessorsOf(block);
    }

    public Collection<BasicBlock> getNormalPredecessorsOf(BasicBlock block) {
        List<BasicBlock> predecessors = new ArrayList<BasicBlock>();
        for (Edge e : graph.getIncomingEdgesOf(block)) {
            if (!e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                predecessors.add(graph.getEdgeSource(e));
            }
        }
        return predecessors;
    }

    public Collection<BasicBlock> getExceptionalPredecessorsOf(BasicBlock block) {
        List<BasicBlock> predecessors = new ArrayList<BasicBlock>();
        for (Edge e : graph.getIncomingEdgesOf(block)) {
            if (e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                predecessors.add(graph.getEdgeSource(e));
            }
        }
        return predecessors;
    }

    public int getPredecessorCountOf(BasicBlock block) {
        return graph.getPredecessorCountOf(block);
    }

    public int getNormalPredecessorCountOf(BasicBlock block) {
        int count = 0;
        for (Edge e : getIncomingEdgesOf(block)) {
            if (!e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                count++;
            }
        }
        return count;
    }

    public BasicBlock getFirstPredecessorOf(BasicBlock block) {
        return graph.getFirstPredecessorOf(block);
    }

    public Collection<BasicBlock> getSuccessorsOf(BasicBlock block) {
        return graph.getSuccessorsOf(block);
    }

    public Collection<BasicBlock> getNormalSuccessorsOf(BasicBlock block) {
        List<BasicBlock> successors = new ArrayList<BasicBlock>();
        for (Edge e : graph.getOutgoingEdgesOf(block)) {
            if (!e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                successors.add(graph.getEdgeTarget(e));
            }
        }
        return successors;
    }

    public Collection<BasicBlock> getExceptionalSuccessorsOf(BasicBlock block) {
        List<BasicBlock> successors = new ArrayList<BasicBlock>();
        for (Edge e : graph.getOutgoingEdgesOf(block)) {
            if (e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                successors.add(graph.getEdgeTarget(e));
            }
        }
        return successors;
    }

    public int getSuccessorCountOf(BasicBlock block) {
        return graph.getSuccessorCountOf(block);
    }

    public int getNormalSuccessorCountOf(BasicBlock block) {
        int count = 0;
        for (Edge e : getOutgoingEdgesOf(block)) {
            if (!e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                count++;
            }
        }
        return count;
    }

    public BasicBlock getFirstSuccessorOf(BasicBlock block) {
        return graph.getFirstSuccessorOf(block);
    }

    /**
     * Remove unreachable basic blocks. A basic block is unreachable if it has no
     * predecessors et no successors.
     */
    public boolean removeUnreachableBlocks() {
        boolean removed = false;
        for (BasicBlock block : new HashSet<BasicBlock>(graph.getVertices())) {
            if (!block.equals(entryBlock) && !block.equals(exitBlock)
                    && graph.getIncomingEdgesOf(block).isEmpty()
                    && graph.getOutgoingEdgesOf(block).isEmpty()) {
                graph.removeVertex(block);
                removed = true;

                LOGGER.debug("Remove unreachable block {}", block);
            }
        }
        return removed;
    }

    /**
     * Remove unnecessary basic blocks.
     */
    public boolean removeUnnecessaryBlock() {
        Set<BasicBlock> toRemove = new HashSet<BasicBlock>();
        for (BasicBlock bb : graph.getVertices()) {
            if (getNormalSuccessorCountOf(bb) == 1) {
                if ((getPredecessorCountOf(bb) > 1
                        && !getFirstNormalOutgoingEdgeOf(bb).hasAttribute(EdgeAttribute.LOOP_BACK_EDGE))
                        || getPredecessorCountOf(bb) == 1) {
                    boolean remove = true;
                    IRInstSeq Insts = bb.getInstructions();
                    if (Insts != null) {
                        for (IRInst inst : Insts) {
                            if (!inst.isIgnored()) {
                                remove = false;
                                break;
                            }
                        }
                    }
                    if (remove) {
                        toRemove.add(bb);
                    }
                }
            }
        }

        for (BasicBlock bb : toRemove) {
            Edge outgoingEdge = getFirstNormalOutgoingEdgeOf(bb);
            BasicBlock successor = graph.getEdgeTarget(outgoingEdge);
            Collection<Edge> incomingEdges = graph.getIncomingEdgesOf(bb);
            for (Edge incomingEdge : new ArrayList<Edge>(incomingEdges)) {
                BasicBlock predecessor = graph.getEdgeSource(incomingEdge);
                graph.removeEdge(incomingEdge);
                graph.addEdge(predecessor, successor, incomingEdge);
            }
            graph.removeEdge(outgoingEdge);
            graph.removeVertex(bb);

            // move attributes and data to the successor
            successor.putProperties(bb.getProperties());

            LOGGER.debug("Remove unnecessary BB {}", bb);
        }

        return toRemove.size() > 0;
    }

    public List<BasicBlock> getOtherExits() {
        List<BasicBlock> otherExits = new ArrayList<BasicBlock>();
        for (BasicBlock bb : getBasicBlocks()) {
            if (!bb.equals(exitBlock) && getSuccessorCountOf(bb) == 0) {
                otherExits.add(bb);
            }
        }
        return otherExits;
    }

    public boolean removeDanglingBlocks() {
        Tree<BasicBlock, Edge> tree = graph.getReversePostOrderDFST(exitBlock, true);
        Set<BasicBlock> toRemove = new HashSet<BasicBlock>(graph.getVertices());
        toRemove.removeAll(tree.getNodes());
        for (BasicBlock bb : toRemove) {
            graph.removeVertex(bb);
            LOGGER.debug("Remove dangling BB {}", bb);
        }
        return toRemove.size() > 0;
    }

    /**
     * Remove critical edges. A critical edge is an edge which is neither the
     * only edge leaving its source block, nor the only edge entering its
     * destination block.
     * Those edges must be split (a new block must be created in the middle of
     * the edge) in order to insert computations on the edge without affecting
     * any other edges.
     */
    public boolean removeCriticalEdges() {
        List<Edge> criticalEdges = new ArrayList<Edge>();

        // find critical edges
        for (Edge e : graph.getEdges()) {
            BasicBlock source = graph.getEdgeSource(e);
            BasicBlock target = graph.getEdgeTarget(e);
            if (graph.getSuccessorCountOf(source) > 1
                    && graph.getPredecessorCountOf(target) > 1
                    && !e.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                criticalEdges.add(e);
            }
        }

        // remove critical edges
        for (Edge criticalEdge : criticalEdges) {
            LOGGER.debug("Remove critical edge {}", graph.toString(criticalEdge));
            BasicBlock source = graph.getEdgeSource(criticalEdge);
            BasicBlock target = graph.getEdgeTarget(criticalEdge);
            removeEdge(criticalEdge);
            BasicBlock emptyBlock = BasicBlockImpl.createEmpty();
            emptyBlock.setInstructions(new IRInstSeq());
            graph.addVertex(emptyBlock);
            addEdge(source, emptyBlock, criticalEdge);
            Edge newEdge = EDGE_FACTORY.createEdge();
            if (criticalEdge.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
                newEdge.addAttribute(EdgeAttribute.EXCEPTIONAL_EDGE);
            }
            addEdge(emptyBlock, target, newEdge);
            // same exception handlers for the ne empty block than the source
            // block of the critical edge (in order not to break try regions)
            for (Edge e : getExceptionalOutgoingEdgesOf(source)) {
                BasicBlock handlerEntry = graph.getEdgeTarget(e);
                if (handlerEntry.getType() == BasicBlockType.EMPTY) {
                    handlerEntry = graph.getFirstSuccessorOf(handlerEntry);
                }
                addEdge(emptyBlock, handlerEntry, true);
            }
        }

        return criticalEdges.size() > 0;
    }

    void performDepthFirstSearch() {
        // build depth first spanning Tree
        dfst = graph.getReversePostOrderDFST(entryBlock, false);
        int order = 0;
        for (BasicBlock block : dfst.getNodes()) {
            block.setOrder(order++);
        }

        LOGGER.trace("Perform reverse post order DFS");
        LOGGER.trace("DFST : \n{}", Trees.toString(dfst));
    }

    private void analyseEdgeCategory() {
        Matrix<Boolean> ancestorsMatrix = Trees.calculateAncestorsMatrix(dfst);

        for (Edge e : graph.getEdges()) {
            BasicBlock source = graph.getEdgeSource(e);
            BasicBlock target = graph.getEdgeTarget(e);
            if (ancestorsMatrix.getValue(source.getOrder(), target.getOrder()) ||
                ancestorsMatrix.getValue(target.getOrder(), source.getOrder())) {
                if (target.getOrder() > source.getOrder()) {
                    e.setCategory(EdgeCategory.ADVANCING);
                } else if (target.getOrder() < source.getOrder()) {
                    if (dominatorInfo.dominates(target, source)) {
                        e.setCategory(EdgeCategory.BACK);
                    } else {
                        e.setCategory(EdgeCategory.RETREATING);
                    }
                }
            } else {
                e.setCategory(EdgeCategory.CROSS);
            }

            LOGGER.trace("Edge Category of {} : {}", graph.toString(e), e.getCategory());
        }
    }

    private void analyseNaturalLoops() {
        naturalLoops.clear();

        analyseEdgeCategory();
        for (Edge e : graph.getEdges()) {
            switch (e.getCategory()) {
                case BACK: {
                    BasicBlock head = graph.getEdgeTarget(e);
                    BasicBlock v = graph.getEdgeSource(e);
                    Set<BasicBlock> visited = new HashSet<BasicBlock>();
                    visited.add(head);
                    List<BasicBlock> body = new ArrayList<BasicBlock>();
                    body.add(head);
                    graph.reversePostOrderDFS(v, visited, body, null, true);
                    // order blocks by dominance
                    Collections.sort(body, new Comparator<BasicBlock>() {
                        @Override
                        public int compare(BasicBlock bb1, BasicBlock bb2) {
                            return dominatorInfo.dominates(bb1, bb2) ? -1 : 1;
                        }
                    });
                    NaturalLoop nl = new NaturalLoop(this, e, body);
                    naturalLoops.put(head, nl);
                    e.addAttribute(EdgeAttribute.LOOP_BACK_EDGE);
                    LOGGER.debug(" Found natural loop : {}", nl);
                    break;
                }

                case RETREATING:
                    LOGGER.warn("Irreducible control flow detected");
                    break;
            }
        }

        // self loops
        for (Edge edge : graph.getEdges()) {
            BasicBlock source = graph.getEdgeSource(edge);
            BasicBlock target = graph.getEdgeTarget(edge);
            if (source.equals(target)) {
                edge.addAttribute(EdgeAttribute.SELF_LOOP_EDGE);
                edge.addAttribute(EdgeAttribute.LOOP_BACK_EDGE);

                NaturalLoop nl = new NaturalLoop(this, edge, Collections.singletonList(source));
                naturalLoops.put(nl.getHead(), nl);
                LOGGER.debug(" Found self loop : {}", nl);
            }
        }
    }

    private void buildLoopTree(NaturalLoop parent, List<BasicBlock> loopBody) {
        while (loopBody.size() > 0) {
            BasicBlock bb = loopBody.remove(0);
            if (parent != null && bb.equals(parent.getHead())) {
                continue;
            }
            Collection<NaturalLoop> children = naturalLoops.get(bb);
            for (NaturalLoop child : children) {
                if (parent == null) {
                    outermostLoops.add(child);
                } else {
                    child.setParent(parent);
                }
                buildLoopTree(child, new ArrayList<BasicBlock>(child.getBody()));
                loopBody.removeAll(child.getBody());
            }
        }
    }

    public void updateLoopInfo() {
        LOGGER.debug("Update loop info");

        performDepthFirstSearch();

        // reset edge loop attributes
        for (Edge e : getEdges()) {
            e.setCategory(null);
            e.removeAttribute(EdgeAttribute.LOOP_BACK_EDGE);
            e.removeAttribute(EdgeAttribute.LOOP_EXIT_EDGE);
            e.removeAttribute(EdgeAttribute.SELF_LOOP_EDGE);
        }

        // detect natural loops
        analyseNaturalLoops();

        // tag loop exit edges
        for (NaturalLoop nl : naturalLoops.values()) {
            for (Edge exitEdge : nl.getExitEdges()) {
                LOGGER.trace("Loop exit edge {}", graph.toString(exitEdge));
                exitEdge.addAttribute(EdgeAttribute.LOOP_EXIT_EDGE);
            }
        }

        // build loop tree
        outermostLoops.clear();
        Tree<BasicBlock, Edge> domTree = dominatorInfo.getDominatorsTree();
        List<BasicBlock> loopBody = new ArrayList<BasicBlock>(domTree.getNodesPreOrder());
        buildLoopTree(null, loopBody);

        if (outermostLoops.size() > 0) {
            LOGGER.debug("Loop tree :\n{}", NaturalLoop.toString(outermostLoops));
        }
    }

    public boolean mergeNaturalLoops() {
        boolean merged = false;
        for (Map.Entry<BasicBlock, Collection<NaturalLoop>> entry : naturalLoops.asMap().entrySet()) {
            BasicBlock head = entry.getKey();
            Collection<NaturalLoop> naturalLoopsWithSameHeader = entry.getValue();
            if (naturalLoopsWithSameHeader.size() > 1) {
                LOGGER.trace("Merge natural loops {}", naturalLoopsWithSameHeader);
                BasicBlock empty = BasicBlockImpl.createEmpty();
                addBasicBlock(empty);
                addEdge(empty, head).addAttribute(EdgeAttribute.LOOP_BACK_EDGE);
                for (NaturalLoop nl : naturalLoopsWithSameHeader) {
                    Edge oldBackEdge = getEdge(nl.getTail(), nl.getHead());
                    oldBackEdge.removeAttribute(EdgeAttribute.LOOP_BACK_EDGE);
                    removeEdge(oldBackEdge);
                    addEdge(nl.getTail(), empty, oldBackEdge);
                }
                merged = true;
            }
        }
        return merged;
    }

    public void exportPane(Writer writer, String title, int paneId, int indentLevel) throws IOException {
        graph.exportPane(writer, title, paneId, indentLevel, RANGE_GRAPHVIZ_RENDERER,
                         EDGE_GRAPHVIZ_RENDERER);
    }

    public void export(Writer writer,
                       GraphvizRenderer<BasicBlock> bbRenderer,
                       GraphvizRenderer<Edge> edgeRenderer) throws IOException {
        export(writer, name, bbRenderer, edgeRenderer);
    }

    public void export(Writer writer, String title,
                       GraphvizRenderer<BasicBlock> bbRenderer,
                       GraphvizRenderer<Edge> edgeRenderer) throws IOException {
        graph.export(writer, title, bbRenderer, edgeRenderer);
    }

    public void export(Writer writer, String title) throws IOException {
        graph.export(writer, title, RANGE_GRAPHVIZ_RENDERER,
                     EDGE_GRAPHVIZ_RENDERER);
    }

    public void export(Writer writer) throws IOException {
        export(writer, name);
    }

    public void export(String fileName) {
        Writer writer = null;
        try {
            writer = new FileWriter(fileName);
            export(writer);
        } catch (IOException e) {
            LOGGER.error(e.toString(), e);
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    LOGGER.error(e.toString(), e);
                }
            }
        }
    }

    public void exportInst(Writer writer) throws IOException {
        graph.export(writer, name, IR_GRAPHVIZ_RENDERER,
                     EDGE_GRAPHVIZ_RENDERER);
    }

    public LocalVariableTable getLocalVariableTable() {
        return localVariableTable;
    }

    public void setLocalVariableTable(LocalVariableTable localVariableTable) {
        this.localVariableTable = localVariableTable;
    }

    public ExceptionTable getExceptionTable() {
        return exceptionTable;
    }

    public void setExceptionTable(ExceptionTable exceptionTable) {
        this.exceptionTable = exceptionTable;
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
