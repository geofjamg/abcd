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
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.tac.model.ArrayLengthInst;
import fr.jamgotchian.abcd.core.tac.model.AssignConstInst;
import fr.jamgotchian.abcd.core.tac.model.AssignVarInst;
import fr.jamgotchian.abcd.core.tac.model.BinaryInst;
import fr.jamgotchian.abcd.core.tac.model.CallMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CastInst;
import fr.jamgotchian.abcd.core.tac.model.ChoiceInst;
import fr.jamgotchian.abcd.core.tac.model.ConditionalInst;
import fr.jamgotchian.abcd.core.tac.model.DefInst;
import fr.jamgotchian.abcd.core.tac.model.GetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.GetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.GetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.InstanceOfInst;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorEnterInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorExitInst;
import fr.jamgotchian.abcd.core.tac.model.NewArrayInst;
import fr.jamgotchian.abcd.core.tac.model.NewObjectInst;
import fr.jamgotchian.abcd.core.tac.model.PhiInst;
import fr.jamgotchian.abcd.core.tac.model.ReturnInst;
import fr.jamgotchian.abcd.core.tac.model.SetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.SetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.SetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.SwitchInst;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.TACInstSeq;
import fr.jamgotchian.abcd.core.tac.model.TACInstVisitor;
import fr.jamgotchian.abcd.core.tac.model.ThrowInst;
import fr.jamgotchian.abcd.core.tac.model.UnaryInst;
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.tac.util.TACInstWriter;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class FinallyUninliner {

    private static final Logger logger = Logger.getLogger(FinallyUninliner.class.getName());

    static {
        logger.setLevel(Level.FINEST);
    }

    private static class TACComparator implements TACInstVisitor<Boolean, TACInst> {

        private final TACInstSeq otherSeq;

        private TACComparator(TACInstSeq otherSeq) {
            this.otherSeq = otherSeq;
        }

        public Boolean visit(TACInstSeq seq, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(ArrayLengthInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(AssignConstInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(AssignVarInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(BinaryInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(CallMethodInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(CallStaticMethodInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(CastInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(ConditionalInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(GetArrayInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(SetArrayInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(GetFieldInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(SetFieldInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(JumpIfInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(InstanceOfInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(MonitorEnterInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(MonitorExitInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(NewArrayInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(NewObjectInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(ReturnInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(SwitchInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(ThrowInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(UnaryInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(ChoiceInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(PhiInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(GetStaticFieldInst inst, TACInst arg) {
            return null; // TODO
        }

        public Boolean visit(SetStaticFieldInst inst, TACInst arg) {
            return null; // TODO
        }
    }

    private final ControlFlowGraph CFG;

    public FinallyUninliner(ControlFlowGraph CFG) {
        this.CFG = CFG;
    }

    public void uninline() {
        List<DirectedGraph<BasicBlock, Edge>> finallySubgraphs
                = CFG.getFinallySubgraphs();

        for (DirectedGraph<BasicBlock, Edge> finallySubgraph : finallySubgraphs) {
            BasicBlock finallyEntry = finallySubgraph.getEntries().iterator().next();
            BasicBlock finallyExit = finallySubgraph.getExits().iterator().next();
            logger.log(Level.FINEST, "Finally subgraph : entry={0}, exit={1}, vertices={2}",
                    new Object[] {finallyEntry, finallyExit, finallySubgraph.getVertices()});

            AnalysisData data = (AnalysisData) finallyExit.getData();
            TACInstSeq instructions = data.getInstructions();
            TACInst lastInst = instructions.getLast();
            if (!(lastInst instanceof ThrowInst)) {
                throw new ABCDException("Last instruction of finally subgraph " +
                        "exit should be a throw instruction");
            }
            instructions.remove(instructions.size()-1);

            // remove dead variables
            boolean deadVar = true;
            while (deadVar) {
                deadVar = false;
                Map<BasicBlock, Set<Variable>> liveVariables
                        = new LiveVariablesAnalysis(CFG).analyse();
                for (BasicBlock bb : finallySubgraph.getVertices()) {
                    for (Iterator<TACInst> it = ((AnalysisData) bb.getData()).getInstructions().iterator();
                         it.hasNext();) {
                        TACInst inst = it.next();
                        if (inst instanceof AssignVarInst ||
                                inst instanceof AssignConstInst ) {
                            Variable result = ((DefInst) inst).getResult();
                            if (!liveVariables.get(bb).contains(result)) {
                                logger.log(Level.FINEST, "Remove useless instruction {0}",
                                        TACInstWriter.toText(inst));
                                it.remove();
                                deadVar = true;
                            }
                        }
                    }
                }
            }

            // seach for try exits
            Set<BasicBlock> trySubgraph = new HashSet<BasicBlock>();
            for (BasicBlock bb : CFG.getPredecessorsOf(finallyEntry)) {
                trySubgraph.add(bb);
            }

            Set<BasicBlock> tryExits = new HashSet<BasicBlock>();
            for (BasicBlock bb : trySubgraph) {
                for (Edge outgoingEdge : CFG.getOutgoingEdgesOf(bb)) {
                    if (!outgoingEdge.isExceptional()) {
                        BasicBlock successor = CFG.getEdgeTarget(outgoingEdge);
                        if (!trySubgraph.contains(successor)) {
                            tryExits.add(successor);
                        }
                    }
                }
            }

            logger.log(Level.FINEST, "Exit BB for finally with {0} header : {1}",
                    new Object[] {finallyEntry, tryExits});

            // uninline finally subgraph at try exits

            // TODO
        }
    }

    private boolean subgraphEquals(BasicBlock bb1, BasicBlock bb2) {
        TACInstSeq seq1 = ((AnalysisData) bb1.getData()).getInstructions();
        TACInstSeq seq2 = ((AnalysisData) bb2.getData()).getInstructions();
        Boolean equals = seq1.accept(new TACComparator(seq2), null);
        if (Boolean.FALSE.equals(equals)) {
            return false;
        }
        // TODO
        return true;
    }
}
