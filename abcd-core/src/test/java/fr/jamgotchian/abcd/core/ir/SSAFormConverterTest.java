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

import fr.jamgotchian.abcd.core.graph.DominatorInfo;
import fr.jamgotchian.abcd.core.util.Collections3;
import java.util.Arrays;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SSAFormConverterTest {

    private IRInstFactory instFactory;

    private VariableFactory varFactory;

    public SSAFormConverterTest() {
    }

    @Before
    public void setUp() {
        instFactory = new IRInstFactory();
        varFactory = new VariableFactory();
    }

    @After
    public void tearDown() {
        instFactory = null;
        varFactory = null;
    }

    private BasicBlock newBasicBlock(int id, int... vars) {
        BasicBlock bb = new BasicBlockImpl(null, null, "BB" + id);
        for (int var : vars) {
            bb.setInstructions(new IRInstSeq());
            bb.getInstructions().add(instFactory.newAssignInt(varFactory.create(var, bb, -1), 0));
        }
        return bb;
    }

    @Test
    public void test1() {
        BasicBlock bb0 = newBasicBlock(0, 0, 1, 2, 8);
        BasicBlock bb1 = newBasicBlock(1, 0, 2);
        BasicBlock bb2 = newBasicBlock(2, 1, 2, 3);
        BasicBlock bb3 = newBasicBlock(3, 0, 3);
        BasicBlock bb4 = newBasicBlock(4, 3);
        BasicBlock bb5 = newBasicBlock(5, 2);
        BasicBlock bb6 = newBasicBlock(6, 1);
        BasicBlock bb7 = newBasicBlock(7, 8);
        BasicBlock bb8 = newBasicBlock(8, 9);
        ControlFlowGraph graph = new ControlFlowGraph("test", bb0, bb8, null);
        graph.addBasicBlock(bb1);
        graph.addBasicBlock(bb2);
        graph.addBasicBlock(bb3);
        graph.addBasicBlock(bb4);
        graph.addBasicBlock(bb5);
        graph.addBasicBlock(bb6);
        graph.addBasicBlock(bb7);
        Edge e01 = graph.addEdge(bb0, bb1);
        Edge e12 = graph.addEdge(bb1, bb2);
        Edge e13 = graph.addEdge(bb1, bb3);
        Edge e27 = graph.addEdge(bb2, bb7);
        Edge e34 = graph.addEdge(bb3, bb4);
        Edge e35 = graph.addEdge(bb3, bb5);
        Edge e46 = graph.addEdge(bb4, bb6);
        Edge e56 = graph.addEdge(bb5, bb6);
        Edge e67 = graph.addEdge(bb6, bb7);
        Edge e71 = graph.addEdge(bb7, bb1);
        Edge e78 = graph.addEdge(bb7, bb8);
        graph.updateDominatorInfo();
        graph.updatePostDominatorInfo();
        graph.updateLoopInfo();
        DominatorInfo<BasicBlock, Edge> info = graph.getDominatorInfo();

        /*
         *   BB  DF
         *   0   -
         *   1   -
         *   2   7
         *   3   7
         *   4   6
         *   5   6
         *   6   7
         *   7   1
         */
        Assert.assertTrue(info.getDominanceFrontierOf(bb0).isEmpty());
        Assert.assertTrue(info.getDominanceFrontierOf(bb1).isEmpty());
        Assert.assertTrue(Collections3.equals(info.getDominanceFrontierOf2(bb2),
                                              Arrays.asList(bb7)));
        Assert.assertTrue(Collections3.equals(info.getDominanceFrontierOf2(bb3),
                                              Arrays.asList(bb7)));
        Assert.assertTrue(Collections3.equals(info.getDominanceFrontierOf2(bb4),
                                              Arrays.asList(bb6)));
        Assert.assertTrue(Collections3.equals(info.getDominanceFrontierOf2(bb5),
                                              Arrays.asList(bb6)));
        Assert.assertTrue(Collections3.equals(info.getDominanceFrontierOf2(bb6),
                                              Arrays.asList(bb7)));
        Assert.assertTrue(Collections3.equals(info.getDominanceFrontierOf2(bb7),
                                              Arrays.asList(bb1)));

        new SSAFormConverter(graph, instFactory, varFactory).convert();
    }
}
