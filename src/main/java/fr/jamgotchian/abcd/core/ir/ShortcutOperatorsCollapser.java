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

import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ShortcutOperatorsCollapser {

    private static final Logger logger
            = Logger.getLogger(ShortcutOperatorsCollapser.class.getName());

    private final ControlFlowGraph cfg;

    private final TemporaryVariableFactory tmpVarFactory;

    private final IRInstFactory instFactory;

    public ShortcutOperatorsCollapser(ControlFlowGraph cfg,
                                      TemporaryVariableFactory tmpVarFactory,
                                      IRInstFactory instFactory) {
        this.cfg = cfg;
        this.tmpVarFactory = tmpVarFactory;
        this.instFactory = instFactory;
    }

    private void collapseOperator(BasicBlock bb1, BasicBlock bb2,
                                  BasicBlock trueBB2, BasicBlock falseBB2,
                                  IRBinaryOperator operator, boolean invert2) {
        IRInstSeq seq1 = bb1.getInstructions();
        IRInstSeq seq2 = bb2.getInstructions();
        JumpIfInst jumpInst1 = (JumpIfInst) seq1.getLast();
        JumpIfInst jumpInst2 = (JumpIfInst) seq2.getLast();
        seq1.removeLast();
        seq2.removeLast();
        seq1.addAll(seq2);
        seq2.clear();
        Variable cond2;
        if (invert2) {
            cond2 = tmpVarFactory.create(bb1);
            seq1.add(instFactory.newUnary(cond2, IRUnaryOperator.NOT, jumpInst2.getCond()));
        } else {
            cond2 = jumpInst2.getCond();
        }
        Variable agregatedCond = tmpVarFactory.create(bb1);
        seq1.add(instFactory.newBinary(agregatedCond,
                                       operator,
                                       jumpInst1.getCond(),
                                       cond2));
        seq1.add(instFactory.newJumpIf(agregatedCond));
        cfg.removeBasicBlock(bb2);
        cfg.addEdge(bb1, trueBB2).setValue(invert2 ? Boolean.FALSE : Boolean.TRUE);
        cfg.addEdge(bb1, falseBB2).setValue(invert2 ? Boolean.TRUE : Boolean.FALSE);
    }

    private boolean checkAnd(BasicBlock bb1, Edge trueEdge1, Edge falseEdge1) {
        BasicBlock bb2 = cfg.getEdgeTarget(trueEdge1);
        if (bb2.getInstructions().getLast() instanceof JumpIfInst) {
            Edge trueEdge2 = null;
            Edge falseEdge2 = null;
            for (Edge e : cfg.getOutgoingEdgesOf(bb2)) {
                if (e.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                    break;
                } else if (Boolean.TRUE.equals(e.getValue())) {
                    trueEdge2 = e;
                } else if (Boolean.FALSE.equals(e.getValue())) {
                    falseEdge2 = e;
                }
            }
            if (trueEdge2 != null && falseEdge2 != null) {
                BasicBlock falseBB1 = cfg.getEdgeTarget(falseEdge1);
                BasicBlock trueBB2 = cfg.getEdgeTarget(trueEdge2);
                BasicBlock falseBB2 = cfg.getEdgeTarget(falseEdge2);
                if (falseBB2.equals(falseBB1)) {
                    collapseOperator(bb1, bb2, trueBB2, falseBB2, IRBinaryOperator.AND, false);
                    return true;
                } else if (trueBB2.equals(falseBB1)) {
                    collapseOperator(bb1, bb2, trueBB2, falseBB2, IRBinaryOperator.AND, true);
                    return true;
                }
            }
        }
        return false;
    }

    private boolean checkOr(BasicBlock bb1, Edge trueEdge1, Edge falseEdge1) {
        BasicBlock bb2 = cfg.getEdgeTarget(falseEdge1);
        if (bb2.getInstructions().getLast() instanceof JumpIfInst) {
            Edge trueEdge2 = null;
            Edge falseEdge2 = null;
            for (Edge e : cfg.getOutgoingEdgesOf(bb2)) {
                if (e.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                    break;
                } else if (Boolean.TRUE.equals(e.getValue())) {
                    trueEdge2 = e;
                } else if (Boolean.FALSE.equals(e.getValue())) {
                    falseEdge2 = e;
                }
            }
            if (trueEdge2 != null && falseEdge2 != null) {
                BasicBlock trueBB1 = cfg.getEdgeTarget(trueEdge1);
                BasicBlock trueBB2 = cfg.getEdgeTarget(trueEdge2);
                BasicBlock falseBB2 = cfg.getEdgeTarget(falseEdge2);
                if (trueBB2.equals(trueBB1)) {
                    collapseOperator(bb1, bb2, trueBB2, falseBB2, IRBinaryOperator.OR, false);
                    return true;
                } else if (falseBB2.equals(trueBB1)) {
                    collapseOperator(bb1, bb2, trueBB2, falseBB2, IRBinaryOperator.OR, true);
                    return true;
                }
            }
        }
        return false;
    }

    public void collapse() {
        ConsoleUtil.logTitledSeparator(logger, Level.FINE,
                "Collapse shortcut operators of {0}", '=', cfg.getName());

        boolean change = true;
        while (change) {
            change = false;
            for (BasicBlock bb1 : cfg.getDFST().getNodes()) {
                if (bb1.getInstructions().getLast() instanceof JumpIfInst) {
                    Edge trueEdge1 = null;
                    Edge falseEdge1 = null;
                    for (Edge e : cfg.getOutgoingEdgesOf(bb1)) {
                        if (e.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                            break;
                        } else if (Boolean.TRUE.equals(e.getValue())) {
                            trueEdge1 = e;
                        } else if (Boolean.FALSE.equals(e.getValue())) {
                            falseEdge1 = e;
                        }
                    }
                    if (trueEdge1 != null && falseEdge1 != null) {
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
        }
    }
}
