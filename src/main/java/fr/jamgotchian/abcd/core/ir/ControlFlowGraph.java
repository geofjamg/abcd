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

package fr.jamgotchian.abcd.core.ir;

import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.graph.PostDominatorInfo;
import fr.jamgotchian.abcd.core.graph.DominatorInfo;
import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.util.Range;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface ControlFlowGraph {

    void addBasicBlock(BasicBlock block);

    void removeBasicBlock(BasicBlock block);

    Edge addEdge(BasicBlock source, BasicBlock target);

    void addEdge(BasicBlock source, BasicBlock target, Edge edge);

    Edge addEdge(BasicBlock source, BasicBlock target, boolean exceptional);

    boolean removeUnreachableBlocks();

    boolean removeUnnecessaryBlock() ;

    boolean removeCriticalEdges();

    boolean mergeNaturalLoops();

    void updateDominatorInfo();

    void updatePostDominatorInfo();

    void updateLoopInfo();

    boolean containsEdge(BasicBlock source, BasicBlock target);

    BasicBlock getBasicBlock(Range range);

    BasicBlock getBasicBlock(int first, int last);

    int getBasicBlockCount();

    Collection<BasicBlock> getBasicBlocks();

    Collection<BasicBlock> getBasicBlocksWithinRange(Range range);

    Collection<BasicBlock> getBasicBlocksWithinRange(int first, int last);

    boolean containsBasicBlock(BasicBlock bb);

    Tree<BasicBlock, Edge> getDFST();

    DominatorInfo<BasicBlock, Edge> getDominatorInfo();

    PostDominatorInfo<BasicBlock, Edge> getPostDominatorInfo();

    Edge getEdge(BasicBlock source, BasicBlock target);

    BasicBlock getEdgeSource(Edge edge);

    BasicBlock getEdgeTarget(Edge edge);

    Set<Edge> getEdges();

    DirectedGraph<BasicBlock, Edge> getGraph() ;

    BasicBlock getEntryBlock();

    BasicBlock getExitBlock();

    Edge getFirstIncomingEdgeOf(BasicBlock block);

    Edge getFirstNormalIncomingEdgeOf(BasicBlock block);

    Edge getFirstOutgoingEdgeOf(BasicBlock block);

    Edge getFirstNormalOutgoingEdgeOf(BasicBlock block);

    BasicBlock getFirstPredecessorOf(BasicBlock block);

    BasicBlock getFirstSuccessorOf(BasicBlock block);

    Collection<Edge> getIncomingEdgesOf(BasicBlock block);

    String getName();

    Multimap<BasicBlock, NaturalLoop> getNaturalLoops();

    Collection<BasicBlock> getNonEmptyBasicBlocks();

    Collection<Edge> getOutgoingEdgesOf(BasicBlock block);

    Collection<Edge> getNormalOutgoingEdgesOf(BasicBlock block);

    int getPredecessorCountOf(BasicBlock block);

    int getNormalPredecessorCountOf(BasicBlock block);

    Collection<BasicBlock> getPredecessorsOf(BasicBlock block);

    int getSuccessorCountOf(BasicBlock block);

    int getNormalSuccessorCountOf(BasicBlock block);

    Collection<BasicBlock> getSuccessorsOf(BasicBlock block);

    Collection<BasicBlock> getNormalSuccessorsOf(BasicBlock block);

    Collection<BasicBlock> getExceptionalSuccessorsOf(BasicBlock block);

    boolean isBasicBlockReachable(BasicBlock block);

    void removeEdge(Edge edge);

    boolean removeEdge(BasicBlock source, BasicBlock target);

    BasicBlockSplit splitBasicBlockAt(int index);

    String toString(Collection<Edge> edges);

    String toString(Edge edge);

    LocalVariableTable getLocalVariableTable();

    void setLocalVariableTable(LocalVariableTable localVariableTable);

    ExceptionTable getExceptionTable();

    void setExceptionTable(ExceptionTable exceptionTable);

    void export(Writer writer,
                GraphvizRenderer<BasicBlock> bbRenderer,
                GraphvizRenderer<Edge> edgeRenderer) throws IOException;

    void export(Writer writer) throws IOException;

    void export(String fileName);

    void exportInst(Writer writer) throws IOException;

    ControlFlowGraph clone();
}
