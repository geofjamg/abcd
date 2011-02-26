/*
 * Copyright (C) 2011 geo
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

package fr.jamgotchian.abcd.core.region;

import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.Decompiler;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockTestImpl;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraphImpl;
import fr.jamgotchian.abcd.core.util.SimplestFormatter;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.junit.Test;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import static org.junit.Assert.*;

/**
 *
 * @author geo
 */
public class StructuralAnalysisTest {
        
    private static final Logger logger = Logger.getLogger(StructuralAnalysisTest.class.getName());

    @BeforeClass
    public static void setUpClass() throws Exception {
        // root logger configuration
        Logger rootLogger = Logger.getLogger(Decompiler.class.getPackage().getName());
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

    public StructuralAnalysisTest() {
    }

    /**
     *       B1
     *      /  |
     *    @B2  |
     *      \  |
     *       B3
     *      /  \
     *     B4   B5
     *         /  | 
     *        B6  | 
     *         \  |
     *          B7
     */ 
//    @Test
//    public void testFig7_46() {
//        logger.info("testFig7_46");
//        
//        BasicBlock b1 = new BasicBlockTestImpl(1);
//        BasicBlock b2 = new BasicBlockTestImpl(2);
//        BasicBlock b3 = new BasicBlockTestImpl(3);
//        BasicBlock b4 = new BasicBlockTestImpl(4);
//        BasicBlock b5 = new BasicBlockTestImpl(5);
//        BasicBlock b6 = new BasicBlockTestImpl(6);
//        BasicBlock b7 = new BasicBlockTestImpl(7);
//        ControlFlowGraph graph = new ControlFlowGraph("Fig 7.46");
//        graph.addBasicBlock(b1);
//        graph.addBasicBlock(b2);
//        graph.addBasicBlock(b3);
//        graph.addBasicBlock(b4);
//        graph.addBasicBlock(b5);
//        graph.addBasicBlock(b6);
//        graph.addBasicBlock(b7);
//        graph.addEdge(graph.getEntryBlock(), b1);
//        graph.addEdge(b1, b2).setValue(Boolean.TRUE);
//        graph.addEdge(b1, b3).setValue(Boolean.FALSE);
//        graph.addEdge(b2, b2);
//        graph.addEdge(b2, b3);
//        graph.addEdge(b3, b4).setValue(Boolean.TRUE);
//        graph.addEdge(b3, b5).setValue(Boolean.FALSE);
//        graph.addEdge(b5, b6).setValue(Boolean.TRUE);
//        graph.addEdge(b5, b7).setValue(Boolean.FALSE);
//        graph.addEdge(b6, b7);
//        graph.addEdge(b4, graph.getExitBlock());
//        graph.addEdge(b7, graph.getExitBlock());
//        graph.analyse();
//
//        StructuralAnalysis.Result result = new StructuralAnalysis(graph).analyse();
//                
//        assertTrue(result.getRegionGraph().getVertexCount() == 1);
//    }
//    
//    /**
//     *       B1
//     *       |
//     *       B2
//     *     /    \
//     *    B3    B4
//     *     \    / 
//     *       B5
//     *       |
//     *       B6
//     */
//    @Test
//    public void testIfThenElseStruct() {
//        logger.info("testIfThenElseStruct");
//
//        BasicBlock b1 = new BasicBlockTestImpl(1);
//        BasicBlock b2 = new BasicBlockTestImpl(2);
//        BasicBlock b3 = new BasicBlockTestImpl(3);
//        BasicBlock b4 = new BasicBlockTestImpl(4);
//        BasicBlock b5 = new BasicBlockTestImpl(5);
//        BasicBlock b6 = new BasicBlockTestImpl(6);
//        ControlFlowGraph graph = new ControlFlowGraph("testIfThenElseStruct");
//        graph.addBasicBlock(b1);
//        graph.addBasicBlock(b2);
//        graph.addBasicBlock(b3);
//        graph.addBasicBlock(b4);
//        graph.addBasicBlock(b5);
//        graph.addBasicBlock(b6);
//        graph.addEdge(graph.getEntryBlock(), b1);
//        graph.addEdge(b1, b2);
//        graph.addEdge(b2, b3).setValue(Boolean.TRUE);
//        graph.addEdge(b2, b4).setValue(Boolean.FALSE);
//        graph.addEdge(b3, b5);
//        graph.addEdge(b4, b5);
//        graph.addEdge(b5, b6);
//        graph.addEdge(b6, graph.getExitBlock());
//        graph.analyse();
//
//        StructuralAnalysis.Result result = new StructuralAnalysis(graph).analyse();
//                
//        assertTrue(result.getRegionGraph().getVertexCount() == 1);
//    }
//
//    /**
//     *       B1
//     *       |
//     *       B2<-
//     *     / |   |
//     *   B4  B3--
//     */
//    @Test
//    public void testWhileLoopStruct() {
//        logger.info("testWhileLoopStruct");
//
//        BasicBlock b1 = new BasicBlockTestImpl(1);
//        BasicBlock b2 = new BasicBlockTestImpl(2);
//        BasicBlock b3 = new BasicBlockTestImpl(3);
//        BasicBlock b4 = new BasicBlockTestImpl(4);
//        ControlFlowGraph graph = new ControlFlowGraph("testWhileLoopStruct");
//        graph.addBasicBlock(b1);
//        graph.addBasicBlock(b2);
//        graph.addBasicBlock(b3);
//        graph.addBasicBlock(b4);
//        graph.addEdge(graph.getEntryBlock(), b1);
//        graph.addEdge(b1, b2);
//        graph.addEdge(b2, b3);
//        graph.addEdge(b3, b2);
//        graph.addEdge(b2, b4);
//        graph.addEdge(b4, graph.getExitBlock());
//        graph.analyse();
//
//        StructuralAnalysis.Result result = new StructuralAnalysis(graph).analyse();
//                
//        assertTrue(result.getRegionGraph().getVertexCount() == 1);
//    }
//   
//    /**
//     *       B1
//     *       |
//     *       B2<-
//     *     / |   |
//     *   B3  B4  |
//     *     / |   |
//     *   B3  B5--
//     */
//    @Test
//    public void testWhileLoopBreakStruct() {
//        logger.info("testWhileLoopStruct");
//
//        BasicBlock b1 = new BasicBlockTestImpl(1);
//        BasicBlock b2 = new BasicBlockTestImpl(2);
//        BasicBlock b3 = new BasicBlockTestImpl(3);
//        BasicBlock b4 = new BasicBlockTestImpl(4);
//        BasicBlock b5 = new BasicBlockTestImpl(5);
//        ControlFlowGraph graph = new ControlFlowGraph("testWhileLoopStruct");
//        graph.addBasicBlock(b1);
//        graph.addBasicBlock(b2);
//        graph.addBasicBlock(b3);
//        graph.addBasicBlock(b4);
//        graph.addBasicBlock(b5);
//        graph.addEdge(graph.getEntryBlock(), b1);
//        graph.addEdge(b1, b2);
//        graph.addEdge(b2, b3);
//        graph.addEdge(b2, b4);
//        graph.addEdge(b4, b3);
//        graph.addEdge(b4, b5);
//        graph.addEdge(b5, b2);
//        graph.addEdge(b3, graph.getExitBlock());
//        graph.analyse();
//
//        StructuralAnalysis.Result result = new StructuralAnalysis(graph).analyse();
//                
//        assertTrue(result.getRegionGraph().getVertexCount() == 1);
//    }

    /**
     *       B1
     *       |
     *       B2<-
     *     / |   |
     *   B4  B3  |
     *     / |   |
     *   B4  B5--
     *       |
     *       B4
     */
    @Test
    public void testDoWhileLoopStruct() {
        logger.info("testDoWhileLoopStruct");

        BasicBlock b1 = new BasicBlockTestImpl(1);
        BasicBlock b2 = new BasicBlockTestImpl(2);
        BasicBlock b3 = new BasicBlockTestImpl(3);
        BasicBlock b4 = new BasicBlockTestImpl(4);
        BasicBlock b5 = new BasicBlockTestImpl(5);
        ControlFlowGraph graph = new ControlFlowGraphImpl("testDoWhileLoopStruct");
        graph.addBasicBlock(b1);
        graph.addBasicBlock(b2);
        graph.addBasicBlock(b3);
        graph.addBasicBlock(b4);
        graph.addBasicBlock(b5);
        graph.addEdge(graph.getEntryBlock(), b1);
        graph.addEdge(b1, b2);
        graph.addEdge(b2, b3);
        graph.addEdge(b2, b4).setLoopExit(true);
        graph.addEdge(b3, b5);
        graph.addEdge(b3, b4).setLoopExit(true);
        graph.addEdge(b5, b2);
        graph.addEdge(b5, b4);
        graph.addEdge(b4, graph.getExitBlock());
        graph.analyse();

        DirectedGraph<Region, Edge> regionGraph = new StructuralAnalysis(graph).analyse();
                
        assertTrue(regionGraph.getVertexCount() == 1);
    }
}