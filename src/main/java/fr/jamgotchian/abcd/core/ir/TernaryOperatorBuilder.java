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

import fr.jamgotchian.abcd.core.graph.PostDominatorInfo;
import fr.jamgotchian.abcd.core.graph.DominatorInfo;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.common.ABCDException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Replace choice instructions by conditional instructions (ternary operator)
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TernaryOperatorBuilder {

    private static final Logger logger
            = Logger.getLogger(TernaryOperatorBuilder.class.getName());

    private final ControlFlowGraph cfg;

    private final TemporaryVariableFactory tmpVarFactory;

    private final TACInstFactory instFactory;

    public TernaryOperatorBuilder(ControlFlowGraph CFG,
                                  TemporaryVariableFactory tmpVarFactory,
                                  TACInstFactory instFactory) {
        this.cfg = CFG;
        this.tmpVarFactory = tmpVarFactory;
        this.instFactory = instFactory;
    }

    public void build() {
        for (BasicBlock joinBlock : cfg.getDFST()) {
            TACInstSeq joinInsts = joinBlock.getInstructions();

            for (int i = 0; i < joinInsts.size(); i++) {
                TACInst inst = joinInsts.get(i);
                if (!(inst instanceof ChoiceInst)) {
                    continue;
                }
                ChoiceInst choiceInst = (ChoiceInst) inst;

                List<TACInst> replacement = new ArrayList<TACInst>();

                boolean change = true;
                while (change) {
                    change = false;

                    Multimap<BasicBlock, Variable> forkBlocks
                            = HashMultimap.create();
                    for (Variable var : choiceInst.getChoices()) {
                        BasicBlock block = var.getBasicBlock();
                        DominatorInfo<BasicBlock, Edge> domInfo = cfg.getDominatorInfo();
                        BasicBlock forkBlock = domInfo.getDominatorsTree().getParent(block);
                        forkBlocks.put(forkBlock, var);
                    }

                    for (Map.Entry<BasicBlock, Collection<Variable>> entry
                            : forkBlocks.asMap().entrySet()) {
                        BasicBlock forkBlock = entry.getKey();
                        Collection<Variable> vars = entry.getValue();
                        if (forkBlock.getType() == BasicBlockType.JUMP_IF
                                && vars.size() == 2) {
                            Iterator<Variable> it = vars.iterator();
                            Variable var1 = it.next();
                            Variable var2 = it.next();

                            BasicBlock block1 = var1.getBasicBlock();
                            BasicBlock block2 = var2.getBasicBlock();
                            PostDominatorInfo<BasicBlock, Edge> postDomInfo = cfg.getPostDominatorInfo();
                            Edge forkEdge1 = postDomInfo.getPostDominanceFrontierOf(block1).iterator().next();
                            Edge forkEdge2 = postDomInfo.getPostDominanceFrontierOf(block2).iterator().next();
                            Variable thenVar = null;
                            Variable elseVar = null;
                            if (Boolean.TRUE.equals(forkEdge1.getValue())
                                    && Boolean.FALSE.equals(forkEdge2.getValue())) {
                                thenVar = var1;
                                elseVar = var2;
                            } else if (Boolean.FALSE.equals(forkEdge1.getValue())
                                    && Boolean.TRUE.equals(forkEdge2.getValue())) {
                                thenVar = var2;
                                elseVar = var1;
                            }
                            if (thenVar != null && elseVar != null) {
                                JumpIfInst jumpIfInst = (JumpIfInst) forkBlock.getInstructions().getLast();
                                choiceInst.getChoices().remove(thenVar);
                                choiceInst.getChoices().remove(elseVar);
                                Variable condVar = jumpIfInst.getCond().clone();
                                if (choiceInst.getChoices().isEmpty()) {
                                    Variable resultVar = choiceInst.getResult();
                                    ConditionalInst condInst
                                            = instFactory.newConditional(resultVar, condVar, thenVar, elseVar);
                                    logger.log(Level.FINER, "Replace inst at {0} of {1} : {2}",
                                            new Object[]{i, joinBlock, TACInstWriter.toText(condInst)});
                                    replacement.add(condInst);
                                } else {
                                    Variable resultVar = tmpVarFactory.create(forkBlock);
                                    ConditionalInst condInst
                                            = instFactory.newConditional(resultVar, condVar, thenVar, elseVar);
                                    logger.log(Level.FINER, "Insert inst at {0} of {1} : {2}",
                                            new Object[]{i, joinBlock, TACInstWriter.toText(condInst)});
                                    replacement.add(condInst);
                                    choiceInst.getChoices().add(resultVar);
                                }

                                change = true;
                            } else {
                                throw new ABCDException("Conditional instruction building error");
                            }
                        } else if (forkBlock.getType() == BasicBlockType.SWITCH
                                && vars.size() > 2) {
                            throw new ABCDException("TODO");
                        }
                    }
                }

                if (replacement.size() > 0) {
                    joinInsts.remove(i);
                    joinInsts.addAll(i, replacement);
                }
            }
        }
    }
}
