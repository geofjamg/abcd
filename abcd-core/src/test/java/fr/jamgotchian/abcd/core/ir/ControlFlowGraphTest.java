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

import fr.jamgotchian.abcd.core.util.Collections3;
import fr.jamgotchian.abcd.core.util.RangeImpl;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.InsnNode;
import java.util.Arrays;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ControlFlowGraphTest {

    public ControlFlowGraphTest() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    private static ControlFlowGraph createSimpleGraph() {
        InsnList instructions = new InsnList();
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        instructions.add(new InsnNode(Opcodes.ICONST_0));
        return new ControlFlowGraph("", instructions.size(), null);
    }

    @Test
    public void testMinSplit() {
        try {
            ControlFlowGraph graph = createSimpleGraph();
            graph.splitBasicBlockAt(Integer.MIN_VALUE);
            fail("Should throw an exception");
        } catch (Exception exc) {
        }
    }

    @Test
    public void testMaxSplit() {
        try {
            ControlFlowGraph graph = createSimpleGraph();
            graph.splitBasicBlockAt(11);
        } catch (Exception exc) {
            fail("Should not throw an exception");
        }
    }

    @Test
    public void testMaxSplit2() {
        try {
            ControlFlowGraph graph = createSimpleGraph();
            graph.splitBasicBlockAt(12);
            fail("Should throw an exception");
        } catch (Exception exc) {
        }
    }

    @Test
    public void testSplit() {
        ControlFlowGraph graph = createSimpleGraph();
        BasicBlockSplit result = graph.splitBasicBlockAt(5);
        assertTrue(graph.getBasicBlockCount() == 4);
        BasicBlock[] blocks = graph.getNonEmptyBasicBlocks().toArray(new BasicBlock[4]);
        assertTrue(blocks[0].getRange().equals(new RangeImpl(Integer.MIN_VALUE, -1)));
        assertTrue(blocks[1].getRange().equals(new RangeImpl(0, 4)));
        assertTrue(blocks[2].getRange().equals(new RangeImpl(5, 10)));
        assertTrue(result.getBlockBefore() == blocks[1]);
        assertTrue(result.getBlockAfter() == blocks[2]);
        assertTrue(graph.containsEdge(blocks[1], blocks[2]));
        assertTrue(graph.getEdge(blocks[1], blocks[2]) == result.getEdge());
    }

    @Test
    public void testDoubleSplit() {
        ControlFlowGraph graph = createSimpleGraph();
        BasicBlockSplit result1 = graph.splitBasicBlockAt(4);
        BasicBlockSplit result2 = graph.splitBasicBlockAt(10);
        assertTrue(graph.getBasicBlockCount() == 5);
        BasicBlock[] blocks = graph.getNonEmptyBasicBlocks().toArray(new BasicBlock[5]);
        assertTrue(blocks[0].getRange().equals(new RangeImpl(Integer.MIN_VALUE, -1)));
        assertTrue(blocks[1].getRange().equals(new RangeImpl(0, 3)));
        assertTrue(blocks[2].getRange().equals(new RangeImpl(4, 9)));
        assertTrue(blocks[3].getRange().equals(new RangeImpl(10, 10)));
        assertTrue(result1.getBlockBefore() == blocks[1]);
        assertTrue(result2.getBlockBefore() == blocks[2]);
        assertTrue(result2.getBlockAfter() == blocks[3]);
        assertTrue(graph.containsEdge(blocks[1], blocks[2]));
        assertTrue(graph.containsEdge(blocks[2], blocks[3]));
        assertTrue(result1.getEdge() == graph.getEdge(blocks[1], blocks[2]));
        assertTrue(result2.getEdge() == graph.getEdge(blocks[2], blocks[3]));
    }

    @Test
    public void testBugForLoop() {
        InsnList instructions = new InsnList();
        for (int i = 0; i < 21; i++) {
            instructions.add(new InsnNode(Opcodes.ICONST_0));
        }
        ControlFlowGraph graph = new ControlFlowGraph("", instructions.size(), null);
        BasicBlock entryBlock = graph.getEntryBlock();

        BasicBlock block0_20 = graph.getBasicBlock(0, 20);
        assertTrue(block0_20 != null);
        assertTrue(Collections3.equals(graph.getSuccessorsOf(entryBlock), Arrays.asList(block0_20)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block0_20), Arrays.asList(entryBlock)));

        block0_20 = null;
        BasicBlockSplit split8 = graph.splitBasicBlockAt(8);
        BasicBlock block0_7 = split8.getBlockBefore();
        BasicBlock block8_20 = split8.getBlockAfter();
        assertTrue(block0_7 != null);
        assertTrue(block8_20 != null);
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block0_7),
                                               Arrays.asList(entryBlock)));
        assertTrue(Collections3.equals(graph.getSuccessorsOf(block0_7),
                                               Arrays.asList(block8_20)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block8_20),
                                               Arrays.asList(block0_7)));
        assertTrue(graph.getSuccessorsOf(block8_20).isEmpty());
        assertTrue(graph.containsEdge(block0_7, block8_20));

        block8_20 = null;
        BasicBlockSplit split17 = graph.splitBasicBlockAt(17);
        BasicBlock block8_16 = split17.getBlockBefore();
        BasicBlock block17_20 = split17.getBlockAfter();
        assertTrue(block8_16 != null);
        assertTrue(block17_20 != null);
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block0_7),
                                               Arrays.asList(entryBlock)));
        assertTrue(Collections3.equals(graph.getSuccessorsOf(block0_7),
                                               Arrays.asList(block8_16)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block8_16),
                                               Arrays.asList(block0_7)));
        assertTrue(Collections3.equals(graph.getSuccessorsOf(block8_16),
                                               Arrays.asList(block17_20)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block17_20),
                                               Arrays.asList(block8_16)));
        assertTrue(graph.getSuccessorsOf(block17_20).isEmpty());


        graph.addEdge(block0_7, block8_16);
        graph.addEdge(block0_7, block17_20);
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block0_7),
                                               Arrays.asList(entryBlock)));
        assertTrue(Collections3.equals(graph.getSuccessorsOf(block0_7),
                                               Arrays.asList(block8_16, block17_20)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block8_16),
                                               Arrays.asList(block0_7)));
        assertTrue(Collections3.equals(graph.getSuccessorsOf(block8_16),
                                               Arrays.asList(block17_20)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block17_20),
                                               Arrays.asList(block8_16, block0_7)));
        assertTrue(graph.getSuccessorsOf(block17_20).isEmpty());


        block0_7 = null;
        BasicBlockSplit split4 = graph.splitBasicBlockAt(4);
        BasicBlock block0_3 = split4.getBlockBefore();
        BasicBlock block4_7 = split4.getBlockAfter();
        assertTrue(block0_3 != null);
        assertTrue(block4_7 != null);
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block0_3),
                                               Arrays.asList(entryBlock)));
        assertTrue(Collections3.equals(graph.getSuccessorsOf(block0_3),
                                               Arrays.asList(block4_7)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block4_7),
                                               Arrays.asList(block0_3)));
        assertTrue(Collections3.equals(graph.getSuccessorsOf(block4_7),
                                               Arrays.asList(block8_16, block17_20)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block8_16),
                                               Arrays.asList(block4_7)));
        assertTrue(Collections3.equals(graph.getSuccessorsOf(block8_16),
                                               Arrays.asList(block17_20)));
        assertTrue(Collections3.equals(graph.getPredecessorsOf(block17_20),
                                               Arrays.asList(block8_16, block4_7)));
        assertTrue(graph.getSuccessorsOf(block17_20).isEmpty());
    }

}
