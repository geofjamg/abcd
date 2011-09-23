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

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.util.Collections3;
import fr.jamgotchian.abcd.core.util.RangeImpl;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.InsnNode;
import java.util.Arrays;
import java.util.Map;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ControlFlowGraphImplTest {

    public ControlFlowGraphImplTest() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    private static void printDFST(ControlFlowGraph graph) {
        for (BasicBlock block : graph.getDFST().getNodes()) {
            System.out.print(block + " ");
        }
        System.out.println();
    }

    private static void printEdgeCategory(ControlFlowGraph graph) {
        for (Edge e : graph.getEdges()) {
            System.out.println(graph.getEdgeSource(e) + "->" + graph.getEdgeTarget(e) + " : " + e.getCategory());
        }
    }

    private static void printNaturalLoops(ControlFlowGraph graph) {
        for (Map.Entry<BasicBlock, NaturalLoop> entry : graph.getNaturalLoops().entrySet()) {
            System.out.println(entry.getValue());
        }
    }

//    @Test
    public void testDragonBookReducibleCFG() {
        System.out.println("testDragonBookReducibleCFG");
        BasicBlock b1 = new BasicBlockTestImpl("1");
        BasicBlock b2 = new BasicBlockTestImpl("2");
        BasicBlock b3 = new BasicBlockTestImpl("3");
        BasicBlock b4 = new BasicBlockTestImpl("4");
        BasicBlock b5 = new BasicBlockTestImpl("5");
        BasicBlock b6 = new BasicBlockTestImpl("6");
        BasicBlock b7 = new BasicBlockTestImpl("7");
        BasicBlock b8 = new BasicBlockTestImpl("8");
        BasicBlock b9 = new BasicBlockTestImpl("9");
        BasicBlock b10 = new BasicBlockTestImpl("10");
        ControlFlowGraph graph = new ControlFlowGraphImpl("Test", b1);
        graph.addBasicBlock(b2);
        graph.addBasicBlock(b3);
        graph.addBasicBlock(b4);
        graph.addBasicBlock(b5);
        graph.addBasicBlock(b6);
        graph.addBasicBlock(b7);
        graph.addBasicBlock(b8);
        graph.addBasicBlock(b9);
        graph.addBasicBlock(b10);
        graph.addEdge(b1, b3);
        graph.addEdge(b1, b2);
        graph.addEdge(b2, b3);
        graph.addEdge(b3, b4);
        graph.addEdge(b4, b6);
        graph.addEdge(b4, b5);
        graph.addEdge(b5, b7);
        graph.addEdge(b6, b7);
        graph.addEdge(b7, b8);
        graph.addEdge(b8, b10);
        graph.addEdge(b8, b9);
        graph.addEdge(b4, b3);
        graph.addEdge(b7, b4);
        graph.addEdge(b10, b7);
        graph.addEdge(b8, b3);
        graph.addEdge(b9, b1);
        graph.analyseLoops();

        printDFST(graph);
        printEdgeCategory(graph);

        assertArrayEquals(graph.getDFST().getNodes().toArray(),
                          new BasicBlock[] { b1, b2, b3, b4, b5, b6, b7, b8, b9, b10 });

        DominatorInfo<BasicBlock, Edge> dominatorsInfo = graph.getDominatorInfo();

        assertTrue(Collections3.sameContent(Arrays.asList(b1), dominatorsInfo.getDominatorsOf(b1)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b2), dominatorsInfo.getDominatorsOf(b2)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3), dominatorsInfo.getDominatorsOf(b3)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3, b4), dominatorsInfo.getDominatorsOf(b4)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3, b4, b5), dominatorsInfo.getDominatorsOf(b5)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3, b4, b6), dominatorsInfo.getDominatorsOf(b6)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3, b4, b7), dominatorsInfo.getDominatorsOf(b7)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3, b4, b7, b8), dominatorsInfo.getDominatorsOf(b8)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3, b4, b7, b8, b9), dominatorsInfo.getDominatorsOf(b9)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3, b4, b7, b8, b10), dominatorsInfo.getDominatorsOf(b10)));

        Map<BasicBlock, NaturalLoop> naturalLoops = graph.getNaturalLoops();
        printNaturalLoops(graph);
    }

//    @Test
    public void testDragonBookIrreducibleCFG() {
        System.out.println("testDragonBookIrreducibleCFG");
        BasicBlock b1 = new BasicBlockTestImpl("1");
        BasicBlock b2 = new BasicBlockTestImpl("2");
        BasicBlock b3 = new BasicBlockTestImpl("3");
        ControlFlowGraph graph = new ControlFlowGraphImpl("Test", b1);
        graph.addBasicBlock(b2);
        graph.addBasicBlock(b3);
        graph.addEdge(b1, b2);
        graph.addEdge(b1, b3);
        graph.addEdge(b2, b3);
        graph.addEdge(b3, b2);
        try {
            graph.analyseLoops();
            fail("Should throw an exception because of irreducible control flow");
        } catch (ABCDException exc) {
        }

        printDFST(graph);
        printEdgeCategory(graph);

        assertArrayEquals(graph.getDFST().getNodes().toArray(),
                          new BasicBlock[] { b1, b2, b3 });

        DominatorInfo<BasicBlock, Edge> dominatorsInfo = graph.getDominatorInfo();

        assertTrue(Collections3.sameContent(Arrays.asList(b1), dominatorsInfo.getDominatorsOf(b1)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b2), dominatorsInfo.getDominatorsOf(b2)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b3), dominatorsInfo.getDominatorsOf(b3)));

        Map<BasicBlock, NaturalLoop> naturalLoops = graph.getNaturalLoops();
        printNaturalLoops(graph);

        assertTrue(naturalLoops.isEmpty());
    }

//    @Test
    public void testNestedLoopCFG() {
        System.out.println("testNestedLoopCFG");
        BasicBlock b1 = new BasicBlockTestImpl("1");
        BasicBlock b2 = new BasicBlockTestImpl("2");
        BasicBlock b3 = new BasicBlockTestImpl("3");
        BasicBlock b4 = new BasicBlockTestImpl("4");
        BasicBlock b5 = new BasicBlockTestImpl("5");
        BasicBlock b6 = new BasicBlockTestImpl("6");
        BasicBlock b7 = new BasicBlockTestImpl("7");
        ControlFlowGraph graph = new ControlFlowGraphImpl("Test", b1);
        graph.addBasicBlock(b2);
        graph.addBasicBlock(b3);
        graph.addBasicBlock(b4);
        graph.addBasicBlock(b5);
        graph.addBasicBlock(b6);
        graph.addBasicBlock(b7);
        graph.addEdge(b1, b2);
        graph.addEdge(b2, b3);
        graph.addEdge(b2, b4);
        graph.addEdge(b3, b5);
        graph.addEdge(b5, b6);
        graph.addEdge(b5, b7);
        graph.addEdge(b6, b5);
        graph.addEdge(b7, b2);
        graph.analyseLoops();

        printDFST(graph);
        printEdgeCategory(graph);

        assertArrayEquals(graph.getDFST().getNodes().toArray(),
                          new BasicBlock[] { b1, b2, b4, b3, b5, b7, b6 });

        DominatorInfo<BasicBlock, Edge> dominatorsInfo = graph.getDominatorInfo();

        assertTrue(Collections3.sameContent(Arrays.asList(b1), dominatorsInfo.getDominatorsOf(b1)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b2), dominatorsInfo.getDominatorsOf(b2)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b2, b3), dominatorsInfo.getDominatorsOf(b3)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b2, b4), dominatorsInfo.getDominatorsOf(b4)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b2, b3, b5), dominatorsInfo.getDominatorsOf(b5)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b2, b3, b5, b6), dominatorsInfo.getDominatorsOf(b6)));
        assertTrue(Collections3.sameContent(Arrays.asList(b1, b2, b3, b5, b7), dominatorsInfo.getDominatorsOf(b7)));

        Map<BasicBlock, NaturalLoop> naturalLoops = graph.getNaturalLoops();
        printNaturalLoops(graph);

        assertTrue(Collections3.sameContent(naturalLoops.keySet(), Arrays.asList(b2, b5)));
        assertTrue(Collections3.sameContent(Arrays.asList(b2, b3, b5, b6, b7), naturalLoops.get(b2).getBody()));
        assertTrue(Collections3.sameContent(Arrays.asList(b5, b6), naturalLoops.get(b5).getBody()));
    }

//    @Test
    public void testObfuscatedCFG() {
        System.out.println("testObfuscatedCFG");
        BasicBlock b1 = new BasicBlockTestImpl("1");
        BasicBlock b2 = new BasicBlockTestImpl("2");
        BasicBlock b3 = new BasicBlockTestImpl("3");
        BasicBlock b4 = new BasicBlockTestImpl("4");
        BasicBlock b5 = new BasicBlockTestImpl("5");
        BasicBlock b6 = new BasicBlockTestImpl("6");
        BasicBlock b7 = new BasicBlockTestImpl("7");
        BasicBlock b8 = new BasicBlockTestImpl("8");
        BasicBlock b9 = new BasicBlockTestImpl("9");
        BasicBlock b10 = new BasicBlockTestImpl("10");
        BasicBlock b11 = new BasicBlockTestImpl("11");
        BasicBlock b12 = new BasicBlockTestImpl("12");
        BasicBlock b13 = new BasicBlockTestImpl("13");
        BasicBlock b14 = new BasicBlockTestImpl("14");
        ControlFlowGraph graph = new ControlFlowGraphImpl("Test", b1);
        graph.addBasicBlock(b2);
        graph.addBasicBlock(b3);
        graph.addBasicBlock(b4);
        graph.addBasicBlock(b5);
        graph.addBasicBlock(b6);
        graph.addBasicBlock(b7);
        graph.addBasicBlock(b8);
        graph.addBasicBlock(b9);
        graph.addBasicBlock(b10);
        graph.addBasicBlock(b11);
        graph.addBasicBlock(b12);
        graph.addBasicBlock(b13);
        graph.addBasicBlock(b14);
        graph.addEdge(b1, b2);
        graph.addEdge(b2, b13);
        graph.addEdge(b2, b3);
        graph.addEdge(b9, b2);
        graph.addEdge(b3, b8);
        graph.addEdge(b3, b4);
        graph.addEdge(b4, b5);
        graph.addEdge(b4, b14);
        graph.addEdge(b5, b6);
        graph.addEdge(b5, b7);
        graph.addEdge(b6, b8);
        graph.addEdge(b8, b9);
        graph.addEdge(b9, b10);
        graph.addEdge(b9, b11);
        graph.addEdge(b10, b12);
        graph.addEdge(b11, b12);
        graph.addEdge(b12, b13);
        graph.addEdge(b13, b14);
        graph.analyseLoops();

        printEdgeCategory(graph);
        printNaturalLoops(graph);
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
        return new ControlFlowGraphImpl("", instructions);
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
        ControlFlowGraph graph = new ControlFlowGraphImpl("", instructions);
        BasicBlock entryBlock = graph.getEntryBlock();

        BasicBlock block0_20 = graph.getBasicBlock(0, 20);
        assertTrue(block0_20 != null);
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(entryBlock), Arrays.asList(block0_20)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block0_20), Arrays.asList(entryBlock)));

        block0_20 = null;
        BasicBlockSplit split8 = graph.splitBasicBlockAt(8);
        BasicBlock block0_7 = split8.getBlockBefore();
        BasicBlock block8_20 = split8.getBlockAfter();
        assertTrue(block0_7 != null);
        assertTrue(block8_20 != null);
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block0_7),
                                               Arrays.asList(entryBlock)));
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(block0_7),
                                               Arrays.asList(block8_20)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block8_20),
                                               Arrays.asList(block0_7)));
        assertTrue(graph.getSuccessorsOf(block8_20).isEmpty());
        assertTrue(graph.containsEdge(block0_7, block8_20));

        block8_20 = null;
        BasicBlockSplit split17 = graph.splitBasicBlockAt(17);
        BasicBlock block8_16 = split17.getBlockBefore();
        BasicBlock block17_20 = split17.getBlockAfter();
        assertTrue(block8_16 != null);
        assertTrue(block17_20 != null);
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block0_7),
                                               Arrays.asList(entryBlock)));
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(block0_7),
                                               Arrays.asList(block8_16)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block8_16),
                                               Arrays.asList(block0_7)));
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(block8_16),
                                               Arrays.asList(block17_20)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block17_20),
                                               Arrays.asList(block8_16)));
        assertTrue(graph.getSuccessorsOf(block17_20).isEmpty());


        graph.addEdge(block0_7, block8_16);
        graph.addEdge(block0_7, block17_20);
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block0_7),
                                               Arrays.asList(entryBlock)));
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(block0_7),
                                               Arrays.asList(block8_16, block17_20)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block8_16),
                                               Arrays.asList(block0_7)));
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(block8_16),
                                               Arrays.asList(block17_20)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block17_20),
                                               Arrays.asList(block8_16, block0_7)));
        assertTrue(graph.getSuccessorsOf(block17_20).isEmpty());


        block0_7 = null;
        BasicBlockSplit split4 = graph.splitBasicBlockAt(4);
        BasicBlock block0_3 = split4.getBlockBefore();
        BasicBlock block4_7 = split4.getBlockAfter();
        assertTrue(block0_3 != null);
        assertTrue(block4_7 != null);
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block0_3),
                                               Arrays.asList(entryBlock)));
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(block0_3),
                                               Arrays.asList(block4_7)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block4_7),
                                               Arrays.asList(block0_3)));
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(block4_7),
                                               Arrays.asList(block8_16, block17_20)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block8_16),
                                               Arrays.asList(block4_7)));
        assertTrue(Collections3.sameContent(graph.getSuccessorsOf(block8_16),
                                               Arrays.asList(block17_20)));
        assertTrue(Collections3.sameContent(graph.getPredecessorsOf(block17_20),
                                               Arrays.asList(block8_16, block4_7)));
        assertTrue(graph.getSuccessorsOf(block17_20).isEmpty());
    }

}
