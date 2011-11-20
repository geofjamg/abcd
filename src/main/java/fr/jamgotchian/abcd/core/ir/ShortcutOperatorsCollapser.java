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

    private void changeBB(IRInstSeq seq, BasicBlock newBB) {
        for (IRInst inst : seq) {
            if (inst instanceof DefInst) {
                ((DefInst) inst).getResult().setBasicBlock(newBB);
            }
            for (Variable v : inst.getUses()) {
                v.setBasicBlock(newBB);
            }
        }
    }

    private void collapseAndOr(BasicBlock bb1, BasicBlock bb2,
                               BasicBlock trueBB2, BasicBlock falseBB2,
                               IRBinaryOperator operator, boolean invert2) {
        IRInstSeq seq1 = bb1.getInstructions();
        IRInstSeq seq2 = bb2.getInstructions();
        JumpIfInst jumpInst1 = (JumpIfInst) seq1.getLast();
        JumpIfInst jumpInst2 = (JumpIfInst) seq2.getLast();
        // all instructions of block 2 will be moved to block 1, so change
        // basic block link of block 2 variables
        changeBB(seq2, bb1);
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
        Variable newCond = tmpVarFactory.create(bb1);
        seq1.add(instFactory.newBinary(newCond,
                                       operator,
                                       jumpInst1.getCond(),
                                       cond2));
        seq1.add(instFactory.newJumpIf(newCond));
        cfg.removeBasicBlock(bb2);
        cfg.addEdge(bb1, trueBB2).setValue(invert2 ? Boolean.FALSE : Boolean.TRUE);
        cfg.addEdge(bb1, falseBB2).setValue(invert2 ? Boolean.TRUE : Boolean.FALSE);
    }

    /**
     * case 1 :
     *
     *         C1
     *       t/  f\
     *       C2    |    <=> C1 && C2
     *    t/   f\  |
     *
     * case 2 :
     *
     *         C1
     *       t/  f\
     *       C2    |    <=> C1 && !C2
     *    f/   t\  |
     */
    private boolean checkAnd(BasicBlock bb1, Edge trueEdge1, Edge falseEdge1) {
        BasicBlock bb2 = cfg.getEdgeTarget(trueEdge1);
        if (cfg.getPredecessorCountOf(bb2) != 1) {
            return false;
        }
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
                    collapseAndOr(bb1, bb2, trueBB2, falseBB2, IRBinaryOperator.AND, false);
                    return true;
                } else if (trueBB2.equals(falseBB1)) {
                    collapseAndOr(bb1, bb2, trueBB2, falseBB2, IRBinaryOperator.AND, true);
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * case 1 :
     *
     *         C1
     *       f/  t\
     *       C2    |    <=> C1 || C2
     *    f/   t\  |
     *
     * case 2 :
     *
     *         C1
     *       f/  t\
     *       C2    |    <=> C1 || !C2
     *    t/   f\  |
     */
    private boolean checkOr(BasicBlock bb1, Edge trueEdge1, Edge falseEdge1) {
        BasicBlock bb2 = cfg.getEdgeTarget(falseEdge1);
        if (cfg.getPredecessorCountOf(bb2) != 1) {
            return false;
        }
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
                    collapseAndOr(bb1, bb2, trueBB2, falseBB2, IRBinaryOperator.OR, false);
                    return true;
                } else if (falseBB2.equals(trueBB1)) {
                    collapseAndOr(bb1, bb2, trueBB2, falseBB2, IRBinaryOperator.OR, true);
                    return true;
                }
            }
        }
        return false;
    }

    private void collapseTernary(BasicBlock bb1, BasicBlock bb2, BasicBlock bb3,
                                 BasicBlock trueBB2, BasicBlock falseBB2,
                                 BasicBlock trueBB3, BasicBlock falseBB3,
                                 boolean invert2) {
        IRInstSeq seq1 = bb1.getInstructions();
        IRInstSeq seq2 = bb2.getInstructions();
        IRInstSeq seq3 = bb3.getInstructions();
        JumpIfInst jumpInst1 = (JumpIfInst) seq1.getLast();
        JumpIfInst jumpInst2 = (JumpIfInst) seq2.getLast();
        JumpIfInst jumpInst3 = (JumpIfInst) seq3.getLast();
        // all instructions of block 2 and 3 will be moved to block 1, so change
        // basic block link of block 2 and 3 variables
        changeBB(seq2, bb1);
        changeBB(seq3, bb1);
        seq1.removeLast();
        seq2.removeLast();
        seq3.removeLast();
        seq1.addAll(seq2);
        seq1.addAll(seq3);
        seq2.clear();
        seq3.clear();
        Variable cond2;
        if (invert2) {
            cond2 = tmpVarFactory.create(bb1);
            seq1.add(instFactory.newUnary(cond2, IRUnaryOperator.NOT, jumpInst2.getCond()));
        } else {
            cond2 = jumpInst2.getCond();
        }
        Variable newCond = tmpVarFactory.create(bb1);
        seq1.add(instFactory.newConditional(newCond,
                                            jumpInst1.getCond(),
                                            cond2,
                                            jumpInst3.getCond()));
        seq1.add(instFactory.newJumpIf(newCond));
        cfg.removeBasicBlock(bb2);
        cfg.removeBasicBlock(bb3);
        cfg.addEdge(bb1, trueBB2).setValue(invert2 ? Boolean.FALSE : Boolean.TRUE);
        cfg.addEdge(bb1, falseBB2).setValue(invert2 ? Boolean.TRUE : Boolean.FALSE);
    }

    /**
     * case 1 :
     *
     *           C1
     *       t/     f\
     *       C2      C3       <=> C1 ? C2 : C3
     *    t/   f\ t/   f\
     *     |     +      |
     *     |   /   \    |
     *
     * case 2 :
     *           C1
     *       t/     f\
     *       C2      C3       <=> C1 ? !C2 : C3
     *    f/   t\ t/   f\
     *     |     +      |
     *     |   /   \    |
     *
     */
    private boolean checkTernary(BasicBlock bb1, Edge trueEdge1, Edge falseEdge1) {
        BasicBlock bb2 = cfg.getEdgeTarget(trueEdge1);
        BasicBlock bb3 = cfg.getEdgeTarget(falseEdge1);
        if (cfg.getPredecessorCountOf(bb2) != 1
                || cfg.getPredecessorCountOf(bb3) != 1) {
            return false;
        }
        if (!(bb2.getInstructions().getLast() instanceof JumpIfInst)
                || !(bb3.getInstructions().getLast() instanceof JumpIfInst)) {
            return false;
        }
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
        if (trueEdge2 == null || falseEdge2 == null) {
            return false;
        }
        Edge trueEdge3 = null;
        Edge falseEdge3 = null;
        for (Edge e : cfg.getOutgoingEdgesOf(bb3)) {
            if (e.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                break;
            } else if (Boolean.TRUE.equals(e.getValue())) {
                trueEdge3 = e;
            } else if (Boolean.FALSE.equals(e.getValue())) {
                falseEdge3 = e;
            }
        }
        if (trueEdge3 == null || falseEdge3 == null) {
            return false;
        }
        BasicBlock trueBB2 = cfg.getEdgeTarget(trueEdge2);
        BasicBlock falseBB2 = cfg.getEdgeTarget(falseEdge2);
        BasicBlock trueBB3 = cfg.getEdgeTarget(trueEdge3);
        BasicBlock falseBB3 = cfg.getEdgeTarget(falseEdge3);
        if (trueBB2.equals(trueBB3) && falseBB2.equals(falseBB3)) {
            collapseTernary(bb1, bb2, bb3, trueBB2, falseBB2, trueBB3, falseBB3, false);
        } else if (falseBB2.equals(trueBB3) && trueBB2.equals(falseBB3)) {
            collapseTernary(bb1, bb2, bb3, trueBB2, falseBB2, trueBB3, falseBB3, true);
        } else {
            return false;
        }
        return true;
    }

    public boolean collapse() {
        ConsoleUtil.logTitledSeparator(logger, Level.FINE,
                "Collapse shortcut operators of {0}", '=', cfg.getName());

        boolean collapsed = false;

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
                            collapsed = true;
                            break;
                        }
                        if (checkOr(bb1, trueEdge1, falseEdge1)) {
                            change = true;
                            collapsed = true;
                            break;
                        }
                        if (checkTernary(bb1, trueEdge1, falseEdge1)) {
                            change = true;
                            collapsed = true;
                            break;
                        }
                    }
                }
            }
        }

        return collapsed;
    }
}
