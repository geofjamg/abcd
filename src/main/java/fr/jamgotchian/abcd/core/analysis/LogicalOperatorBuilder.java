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

import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.tac.model.TACBinaryOperator;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
import fr.jamgotchian.abcd.core.tac.model.TACInstFactory;
import fr.jamgotchian.abcd.core.tac.model.TACInstSeq;
import fr.jamgotchian.abcd.core.tac.model.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.tac.model.TACUnaryOperator;
import fr.jamgotchian.abcd.core.tac.model.Variable;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LogicalOperatorBuilder {

    private final ControlFlowGraph CFG;

    private final TemporaryVariableFactory tmpVarFactory;

    private final TACInstFactory instFactory;

    public LogicalOperatorBuilder(ControlFlowGraph CFG,
                                  TemporaryVariableFactory tmpVarFactory,
                                  TACInstFactory instFactory) {
        this.CFG = CFG;
        this.tmpVarFactory = tmpVarFactory;
        this.instFactory = instFactory;
    }

    private void agregate(BasicBlock bb1, BasicBlock bb2, BasicBlock trueBB2,
                          BasicBlock falseBB2, TACBinaryOperator operator, boolean invert2) {
        AnalysisData data1 = (AnalysisData) bb1.getData();
        AnalysisData data2 = (AnalysisData) bb2.getData();
        TACInstSeq seq1 = data1.getInstructions();
        TACInstSeq seq2 = data2.getInstructions();
        JumpIfInst jumpInst1 = (JumpIfInst) seq1.getLast();
        JumpIfInst jumpInst2 = (JumpIfInst) seq2.getLast();
        seq1.remove(seq1.size()-1);
        seq2.remove(seq2.size()-1);
        seq1.addAll(seq2);
        seq2.clear();
        Variable cond2;
        if (invert2) {
            cond2 = tmpVarFactory.create(bb1);
            seq1.add(instFactory.newUnary(cond2, TACUnaryOperator.NOT, jumpInst2.getCond()));
        } else {
            cond2 = jumpInst2.getCond();
        }
        Variable agregatedCond = tmpVarFactory.create(bb1);
        seq1.add(instFactory.newBinary(agregatedCond,
                                       operator,
                                       jumpInst1.getCond(),
                                       cond2));
        seq1.add(instFactory.newJumpIf(agregatedCond));
        CFG.removeBasicBlock(bb2);
        CFG.addEdge(bb1, trueBB2).setValue(invert2 ? Boolean.FALSE : Boolean.TRUE);
        CFG.addEdge(bb1, falseBB2).setValue(invert2 ? Boolean.TRUE : Boolean.FALSE);
    }

    private boolean checkAnd(BasicBlock bb1, Edge trueEdge1, Edge falseEdge1) {
        BasicBlock bb2 = CFG.getEdgeTarget(trueEdge1);
        AnalysisData data2 = (AnalysisData) bb2.getData();
        if (data2.getInstructions().getLast() instanceof JumpIfInst) {
            Edge trueEdge2 = null;
            Edge falseEdge2 = null;
            for (Edge e : CFG.getOutgoingEdgesOf(bb2)) {
                if (Boolean.TRUE.equals(e.getValue())) {
                    trueEdge2 = e;
                } else if (Boolean.FALSE.equals(e.getValue())) {
                    falseEdge2 = e;
                }
            }
            BasicBlock falseBB1 = CFG.getEdgeTarget(falseEdge1);
            BasicBlock trueBB2 = CFG.getEdgeTarget(trueEdge2);
            BasicBlock falseBB2 = CFG.getEdgeTarget(falseEdge2);
            if (falseBB2.equals(falseBB1)) {
                agregate(bb1, bb2, trueBB2, falseBB2, TACBinaryOperator.AND, false);
                return true;
            } else if (trueBB2.equals(falseBB1)) {
                agregate(bb1, bb2, trueBB2, falseBB2, TACBinaryOperator.AND, true);
                return true;
            }
        }
        return false;
    }

    private boolean checkOr(BasicBlock bb1, Edge trueEdge1, Edge falseEdge1) {
        BasicBlock bb2 = CFG.getEdgeTarget(falseEdge1);
        AnalysisData data2 = (AnalysisData) bb2.getData();
        if (data2.getInstructions().getLast() instanceof JumpIfInst) {
            Edge trueEdge2 = null;
            Edge falseEdge2 = null;
            for (Edge e : CFG.getOutgoingEdgesOf(bb2)) {
                if (Boolean.TRUE.equals(e.getValue())) {
                    trueEdge2 = e;
                } else if (Boolean.FALSE.equals(e.getValue())) {
                    falseEdge2 = e;
                }
            }

            BasicBlock trueBB1 = CFG.getEdgeTarget(trueEdge1);
            BasicBlock trueBB2 = CFG.getEdgeTarget(trueEdge2);
            BasicBlock falseBB2 = CFG.getEdgeTarget(falseEdge2);
            if (trueBB2.equals(trueBB1)) {
                agregate(bb1, bb2, trueBB2, falseBB2, TACBinaryOperator.OR, false);
                return true;
            } else if (falseBB2.equals(trueBB1)) {
                agregate(bb1, bb2, trueBB2, falseBB2, TACBinaryOperator.OR, true);
                return true;
            }
        }
        return false;
    }

    public void builder() {
        boolean change = true;
        while (change) {
            change = false;
            for (BasicBlock bb1 : CFG.getDFST().getNodes()) {
                AnalysisData data1 = (AnalysisData) bb1.getData();
                if (data1.getInstructions().getLast() instanceof JumpIfInst) {
                    Edge trueEdge1 = null;
                    Edge falseEdge1 = null;
                    for (Edge e : CFG.getOutgoingEdgesOf(bb1)) {
                        if (Boolean.TRUE.equals(e.getValue())) {
                            trueEdge1 = e;
                        } else if (Boolean.FALSE.equals(e.getValue())) {
                            falseEdge1 = e;
                        }
                    }
                    if(checkAnd(bb1, trueEdge1, falseEdge1)) {
                        change = true;
                        break;
                    }
                    if (checkOr(bb1, trueEdge1, falseEdge1)) {
                        change = true;
                        break;
                    }
                }
            }
        }

        CFG.analyseLoops();
    }
}
