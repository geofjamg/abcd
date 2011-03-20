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

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.ast.expr.Constant;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.ForwardDataFlowAnalysis;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class StackSizeAnalysis extends ForwardDataFlowAnalysis<BasicBlock, Edge, Integer> {

    private static class StackEffectAnalyser extends BasicBlockStmtAnalysis {

        private static class DummyExpressionStack implements ExpressionStack {

            private static final Expression DUMMY_EXPR = new Constant(null);

            private int stackSize = 0;

            private int consumption = 0;

            public int getEffect() {
                return Math.max(0, stackSize) - consumption;
            }

            public void push(Expression expr) {
                stackSize++;
            }

            public Expression pop() {
                stackSize--;
                if (stackSize < 0) {
                    consumption++;
                }
                return DUMMY_EXPR;
            }

            public Expression peek() {
                return DUMMY_EXPR;
            }

            public int size() {
                return stackSize;
            }

            public List<Expression> toList() {
                throw new ABCDException("Not implemented");
            }

            @Override
            public ExpressionStack clone() {
                throw new ABCDException("Not implemented");
            }

        }

        public StackEffectAnalyser() {
            super(new DummyExpressionStack());
        }

        @Override
        public void before(BasicBlock block) {
            super.before(block);
            Iterator<Edge> itE = block.getGraph().getIncomingEdgesOf(block).iterator();
            if (itE.hasNext() && itE.next().isExceptional()) {
                pushExpr(new Constant("EXCEPTION"), block);
            }
        }

        @Override
        public void after(BasicBlock block) {
            super.after(block);
            ((BasicBlockAnalysisDataImpl) block.getData()).setStackEffect(((DummyExpressionStack) stack).getEffect());
        }

        @Override
        protected void addStmt(BasicBlock block, Statement stmt) {
            // DO NOTHING
        }
    }

    public StackSizeAnalysis(ControlFlowGraph graph) {
        super(graph.getGraph(), graph.getEntryBlock());
        for (BasicBlock block : graph.getBasicBlocks()) {
            block.visit(new StackEffectAnalyser());
        }
    }

    @Override
    protected Integer getInitValue(BasicBlock block, boolean isStartBlock) {
        return Integer.valueOf(0);
    }

    @Override
    protected Integer combineValues(Integer value1, Integer value2) {
        // value1 and value2 should be equal
        return value1;
    }

    @Override
    protected Integer applyTranferFunction(BasicBlock block, Integer in) {
        return in + ((BasicBlockAnalysisDataImpl) block.getData()).getStackEffect();
    }

    @Override
    protected boolean valuesEqual(Integer value1, Integer value2) {
        return value1.equals(value2);
    }

}

