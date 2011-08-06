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

import fr.jamgotchian.abcd.core.common.LabelManager;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.util.Range;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import org.objectweb.asm.tree.InsnList;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface ControlFlowGraph {

    void addBasicBlock(BasicBlock block);

    void removeBasicBlock(BasicBlock block);

    Edge addEdge(BasicBlock source, BasicBlock target);

    Edge addEdge(BasicBlock source, BasicBlock target, boolean exceptional);

    void compact();

    void removeCriticalEdges();

    void analyseLoops();

    boolean containsEdge(BasicBlock source, BasicBlock target);

    BasicBlock getBasicBlock(Range range);

    BasicBlock getBasicBlock(int first, int last);

    int getBasicBlockCount();

    Collection<BasicBlock> getBasicBlocks();

    Collection<BasicBlock> getBasicBlocksWithinRange(Range range);

    Collection<BasicBlock> getBasicBlocksWithinRange(int first, int last);

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

    Edge getFirstOutgoingEdgeOf(BasicBlock block);

    BasicBlock getFirstPredecessorOf(BasicBlock block);

    BasicBlock getFirstSuccessorOf(BasicBlock block);

    Collection<Edge> getIncomingEdgesOf(BasicBlock block);

    InsnList getInstructions();

    LabelManager getLabelManager();

    String getName();

    Map<BasicBlock, NaturalLoop> getNaturalLoops();

    Collection<BasicBlock> getNonEmptyBasicBlocks();

    Collection<Edge> getOutgoingEdgesOf(BasicBlock block);

    int getPredecessorCountOf(BasicBlock block);

    Collection<BasicBlock> getPredecessorsOf(BasicBlock block);

    int getSuccessorCountOf(BasicBlock block);

    Collection<BasicBlock> getSuccessorsOf(BasicBlock block);

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

    void addFakeEdges();
}
