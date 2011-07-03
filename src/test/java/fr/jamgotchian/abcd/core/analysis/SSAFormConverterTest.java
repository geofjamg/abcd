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
package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.ABCDContext;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockTestImpl;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraphImpl;
import fr.jamgotchian.abcd.core.controlflow.DominatorInfo;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.region.StructuralAnalysisTest;
import fr.jamgotchian.abcd.core.tac.model.IntConst;
import fr.jamgotchian.abcd.core.tac.model.TACInstFactory;
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.util.Collections3;
import fr.jamgotchian.abcd.core.util.SimplestFormatter;
import java.util.Arrays;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SSAFormConverterTest {

    private static final Logger logger = Logger.getLogger(SSAFormConverterTest.class.getName());

    private TACInstFactory instFactory;

    public SSAFormConverterTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
        // root logger configuration
        Logger rootLogger = Logger.getLogger(ABCDContext.class.getPackage().getName());
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new SimplestFormatter());
        handler.setLevel(Level.FINER);
        rootLogger.setLevel(Level.ALL);
        rootLogger.addHandler(handler);
        rootLogger.setUseParentHandlers(false);
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
        Logger rootLogger = Logger.getLogger(StructuralAnalysisTest.class.getPackage().getName());
        for (Handler handler : rootLogger.getHandlers()) {
            handler.close();
        }

        Thread.sleep(1000);
    }

    @Before
    public void setUp() {
        instFactory = new TACInstFactory();
    }

    @After
    public void tearDown() {
        instFactory = null;
    }

    private BasicBlock newBasicBlock(int id, int... vars) {
        BasicBlock bb = new BasicBlockTestImpl("BB", id);
        AnalysisData data = new AnalysisData();
        bb.setData(data);
        for (int var : vars) {
            data.getInstructions().add(instFactory.newAssignConst(new Variable(var, bb, -1),
                                                                  new IntConst(0)));
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
        ControlFlowGraph graph = new ControlFlowGraphImpl("test", bb0, bb7);
        graph.addBasicBlock(bb1);
        graph.addBasicBlock(bb2);
        graph.addBasicBlock(bb3);
        graph.addBasicBlock(bb4);
        graph.addBasicBlock(bb5);
        graph.addBasicBlock(bb6);
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
        graph.analyseLoops();
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
        Assert.assertTrue(Collections3.sameContent(info.getDominanceFrontierOf2(bb2),
                                                   Arrays.asList(bb7)));
        Assert.assertTrue(Collections3.sameContent(info.getDominanceFrontierOf2(bb3),
                                                   Arrays.asList(bb7)));
        Assert.assertTrue(Collections3.sameContent(info.getDominanceFrontierOf2(bb4),
                                                   Arrays.asList(bb6)));
        Assert.assertTrue(Collections3.sameContent(info.getDominanceFrontierOf2(bb5),
                                                   Arrays.asList(bb6)));
        Assert.assertTrue(Collections3.sameContent(info.getDominanceFrontierOf2(bb6),
                                                   Arrays.asList(bb7)));
        Assert.assertTrue(Collections3.sameContent(info.getDominanceFrontierOf2(bb7),
                                                   Arrays.asList(bb1)));

        TACInstFactory factory = new TACInstFactory();
        new SSAFormConverter(graph, factory).convert();
//
//        try {
//            Writer writer = new FileWriter("/tmp/a.dot");
//            try {
//                DOTUtil.writeCFG(graph, null, writer, DOTUtil.DisplayMode.TAC);
//            } finally {
//                writer.close();
//            }
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
    }
}
