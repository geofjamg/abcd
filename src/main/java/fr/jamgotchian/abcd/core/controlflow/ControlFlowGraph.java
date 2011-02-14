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

import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.common.LabelManager;
import fr.jamgotchian.abcd.core.graph.MutableDirectedGraph;
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

    <R> MutableDirectedGraph<R, Edge> createUnexceptionalCFG(RegionFactory<R> factory,
                                                             Map<BasicBlock, R> regions);

    public static interface RegionFactory<R> {
        R create(BasicBlock block);
    }
    
    void addBasicBlock(BasicBlock block);

    Edge addEdge(BasicBlock source, BasicBlock target);

    Edge addEdge(BasicBlock source, BasicBlock target, boolean exceptional);

    void analyse();

    boolean containsEdge(BasicBlock source, BasicBlock target);

    BasicBlock getBasicBlock(Range range);

    BasicBlock getBasicBlock(int first, int last);

    int getBasicBlockCount();

    Collection<BasicBlock> getBasicBlocks();

    Collection<BasicBlock> getBasicBlocksWithinRange(Range range);

    Collection<BasicBlock> getBasicBlocksWithinRange(int first, int last);

    Tree<BasicBlock, Edge> getDFST();

    DominatorInfo getDominatorInfo();

    Edge getEdge(BasicBlock source, BasicBlock target);

    BasicBlock getEdgeSource(Edge edge);

    BasicBlock getEdgeTarget(Edge edge);

    Set<Edge> getEdges();

    BasicBlock getEntryBlock();

    BasicBlock getExitBlock();

    Edge getFirstIncomingEdgesOf(BasicBlock block);

    Edge getFirstOutgoingEdgesOf(BasicBlock block);

    BasicBlock getFirstPredecessorsOf(BasicBlock block);

    BasicBlock getFirstSuccessorsOf(BasicBlock block);

    Collection<Edge> getIncomingEdgesOf(BasicBlock block);

    InsnList getInstructions();

    Multimap<BasicBlock, ForkJoinInfo> getJoinBlocks();

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

    boolean removeEdge(BasicBlock source, BasicBlock target);

    BasicBlockSplit splitBasicBlockAt(int index);

    String toString(Collection<Edge> edges);

    String toString(Edge edge);

    void visit(ControlFlowGraphVisitor visitor);

}
