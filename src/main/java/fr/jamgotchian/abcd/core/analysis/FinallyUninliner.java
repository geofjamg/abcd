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

import com.google.common.base.Objects;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.controlflow.ArrayLengthInst;
import fr.jamgotchian.abcd.core.controlflow.AssignConstInst;
import fr.jamgotchian.abcd.core.controlflow.AssignVarInst;
import fr.jamgotchian.abcd.core.controlflow.BinaryInst;
import fr.jamgotchian.abcd.core.controlflow.CallMethodInst;
import fr.jamgotchian.abcd.core.controlflow.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.controlflow.CastInst;
import fr.jamgotchian.abcd.core.controlflow.ChoiceInst;
import fr.jamgotchian.abcd.core.controlflow.ConditionalInst;
import fr.jamgotchian.abcd.core.controlflow.DefInst;
import fr.jamgotchian.abcd.core.controlflow.GetArrayInst;
import fr.jamgotchian.abcd.core.controlflow.GetFieldInst;
import fr.jamgotchian.abcd.core.controlflow.GetStaticFieldInst;
import fr.jamgotchian.abcd.core.controlflow.InstanceOfInst;
import fr.jamgotchian.abcd.core.controlflow.JumpIfInst;
import fr.jamgotchian.abcd.core.controlflow.MonitorEnterInst;
import fr.jamgotchian.abcd.core.controlflow.MonitorExitInst;
import fr.jamgotchian.abcd.core.controlflow.NewArrayInst;
import fr.jamgotchian.abcd.core.controlflow.NewObjectInst;
import fr.jamgotchian.abcd.core.controlflow.PhiInst;
import fr.jamgotchian.abcd.core.controlflow.ReturnInst;
import fr.jamgotchian.abcd.core.controlflow.SetArrayInst;
import fr.jamgotchian.abcd.core.controlflow.SetFieldInst;
import fr.jamgotchian.abcd.core.controlflow.SetStaticFieldInst;
import fr.jamgotchian.abcd.core.controlflow.SwitchInst;
import fr.jamgotchian.abcd.core.controlflow.TACInst;
import fr.jamgotchian.abcd.core.controlflow.TACInstSeq;
import fr.jamgotchian.abcd.core.controlflow.TACInstVisitor;
import fr.jamgotchian.abcd.core.controlflow.ThrowInst;
import fr.jamgotchian.abcd.core.controlflow.UnaryInst;
import fr.jamgotchian.abcd.core.controlflow.Variable;
import fr.jamgotchian.abcd.core.controlflow.VariableID;
import fr.jamgotchian.abcd.core.controlflow.util.TACInstWriter;
import java.util.HashMap;
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

    private static class VariableComparator {

        /* Variable ID mapping between temporary variables of the 2 sequences */
        private final Map<VariableID, VariableID> mapping
                = new HashMap<VariableID, VariableID>();

        private boolean defEqual(Variable var1, Variable var2) {
            if (mapping.containsKey(var1.getID())
                    || mapping.containsKey(var2.getID())) {
                throw new IllegalStateException("Finally uninlining should be run on SSA form");
            }
            mapping.put(var1.getID(), var2.getID());
            mapping.put(var2.getID(), var1.getID());
            return true;
        }

        private boolean useEqual(Variable var1, Variable var2) {
            if (var1.getID().equals(var2.getID())) {
                return true;
            }
            boolean a =  var2.getID().equals(mapping.get(var1.getID()));
            return a;
        }

        private boolean usesEqual(List<Variable> vars1, List<Variable> vars2) {
            if (vars1.size() != vars2.size()) {
                return false;
            }
            for (int i = 0; i < vars1.size(); i++) {
                if (!useEqual(vars1.get(i), vars2.get(i))) {
                    return false;
                }
            }
            return true;
        }

        private boolean usesEqual(Set<Variable> vars1, Set<Variable> vars2) {
            if (vars1.size() != vars2.size()) {
                return false;
            }
            for (Variable var1 : vars1) {
                boolean found = false;
                for (Variable var2 : vars2) {
                    if (useEqual(var1, var2)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    return false;
                }
            }
            return true;
        }
    }

    private static class TACComparator implements TACInstVisitor<Boolean, TACInst> {

        private final TACInstSeq otherSeq;

        private final VariableComparator comparator;

        private TACComparator(TACInstSeq otherSeq, VariableComparator comparator) {
            this.otherSeq = otherSeq;
            this.comparator = comparator;
        }

        public Boolean visit(TACInstSeq seq, TACInst arg) {
            if (seq.size() != otherSeq.size()) {
                return Boolean.FALSE;
            }
            for (int i = 0; i < seq.size(); i++) {
                TACInst inst = seq.get(i);
                TACInst inst2 = otherSeq.get(i);
                if (Boolean.FALSE.equals(inst.accept(this, inst2))) {
                    return Boolean.FALSE;
                }
            }
            return Boolean.TRUE;
        }

        public Boolean visit(ArrayLengthInst inst, TACInst arg) {
            if (!(arg instanceof ArrayLengthInst)) {
                return Boolean.FALSE;
            }
            ArrayLengthInst inst2 = (ArrayLengthInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && comparator.useEqual(inst.getArray(), inst2.getArray());
        }

        public Boolean visit(AssignConstInst inst, TACInst arg) {
            if (!(arg instanceof AssignConstInst)) {
                return Boolean.FALSE;
            }
            AssignConstInst inst2 = (AssignConstInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && inst.getConst().equals(inst2.getConst());
        }

        public Boolean visit(AssignVarInst inst, TACInst arg) {
            if (!(arg instanceof AssignVarInst)) {
                return Boolean.FALSE;
            }
            AssignVarInst inst2 = (AssignVarInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && comparator.useEqual(inst.getValue(), inst2.getValue());
        }

        public Boolean visit(BinaryInst inst, TACInst arg) {
            if (!(arg instanceof BinaryInst)) {
                return Boolean.FALSE;
            }
            BinaryInst inst2 = (BinaryInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && comparator.useEqual(inst.getLeft(), inst2.getLeft())
                    && comparator.useEqual(inst.getRight(), inst2.getRight())
                    && inst.getOperator() == inst2.getOperator();
        }

        public Boolean visit(CallMethodInst inst, TACInst arg) {
            if (!(arg instanceof CallMethodInst)) {
                return Boolean.FALSE;
            }
            CallMethodInst inst2 = (CallMethodInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && comparator.useEqual(inst.getObject(), inst2.getObject())
                    && inst.getSignature().equals(inst2.getSignature())
                    && comparator.usesEqual(inst.getArguments(), inst2.getArguments());
        }

        public Boolean visit(CallStaticMethodInst inst, TACInst arg) {
            if (!(arg instanceof CallStaticMethodInst)) {
                return Boolean.FALSE;
            }
            CallStaticMethodInst inst2 = (CallStaticMethodInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && inst.getScope().equals(inst2.getScope())
                    && inst.getSignature().equals(inst2.getSignature())
                    && comparator.usesEqual(inst.getArguments(), inst2.getArguments());
        }

        public Boolean visit(CastInst inst, TACInst arg) {
            if (!(arg instanceof CastInst)) {
                return Boolean.FALSE;
            }
            CastInst inst2 = (CastInst) arg;
           return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && inst.getCastType().equals(inst2.getCastType())
                    && comparator.useEqual(inst.getVar(), inst2.getVar());
        }

        public Boolean visit(ConditionalInst inst, TACInst arg) {
            if (!(arg instanceof ConditionalInst)) {
                return Boolean.FALSE;
            }
            ConditionalInst inst2 = (ConditionalInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && comparator.useEqual(inst.getCond(), inst2.getCond())
                    && comparator.useEqual(inst.getThen(), inst2.getThen())
                    && comparator.useEqual(inst.getElse(), inst2.getElse());
        }

        public Boolean visit(GetArrayInst inst, TACInst arg) {
            if (!(arg instanceof GetArrayInst)) {
                return Boolean.FALSE;
            }
            GetArrayInst inst2 = (GetArrayInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && comparator.useEqual(inst.getArray(), inst2.getArray())
                    && comparator.useEqual(inst.getIndex(), inst2.getIndex());
        }

        public Boolean visit(SetArrayInst inst, TACInst arg) {
            if (!(arg instanceof SetArrayInst)) {
                return Boolean.FALSE;
            }
            SetArrayInst inst2 = (SetArrayInst) arg;
            return comparator.useEqual(inst.getArray(), inst2.getArray())
                    && comparator.useEqual(inst.getIndex(), inst2.getIndex())
                    && comparator.useEqual(inst.getValue(), inst2.getValue());
        }

        public Boolean visit(GetFieldInst inst, TACInst arg) {
            if (!(arg instanceof GetFieldInst)) {
                return Boolean.FALSE;
            }
            GetFieldInst inst2 = (GetFieldInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && comparator.useEqual(inst.getObject(), inst2.getObject())
                    && inst.getFieldName().equals(inst2.getFieldName());
        }

        public Boolean visit(SetFieldInst inst, TACInst arg) {
            if (!(arg instanceof SetFieldInst)) {
                return Boolean.FALSE;
            }
            SetFieldInst inst2 = (SetFieldInst) arg;
            return comparator.useEqual(inst.getObject(), inst2.getObject())
                    && inst.getFieldName().equals(inst2.getFieldName())
                    && comparator.defEqual(inst.getValue(), inst2.getValue());
        }

        public Boolean visit(JumpIfInst inst, TACInst arg) {
            if (!(arg instanceof JumpIfInst)) {
                return Boolean.FALSE;
            }
            JumpIfInst inst2 = (JumpIfInst) arg;
            return comparator.useEqual(inst.getCond(), inst2.getCond());
        }

        public Boolean visit(InstanceOfInst inst, TACInst arg) {
            if (!(arg instanceof InstanceOfInst)) {
                return Boolean.FALSE;
            }
            InstanceOfInst inst2 = (InstanceOfInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && inst.getType().equals(inst2.getType())
                    && comparator.useEqual(inst.getVar(), inst2.getVar());
        }

        public Boolean visit(MonitorEnterInst inst, TACInst arg) {
            if (!(arg instanceof MonitorEnterInst)) {
                return Boolean.FALSE;
            }
            MonitorEnterInst inst2 = (MonitorEnterInst) arg;
            return comparator.useEqual(inst.getObj(), inst2.getObj());
        }

        public Boolean visit(MonitorExitInst inst, TACInst arg) {
            if (!(arg instanceof MonitorExitInst)) {
                return Boolean.FALSE;
            }
            MonitorExitInst inst2 = (MonitorExitInst) arg;
            return comparator.useEqual(inst.getObj(), inst2.getObj());
        }

        public Boolean visit(NewArrayInst inst, TACInst arg) {
            if (!(arg instanceof NewArrayInst)) {
                return Boolean.FALSE;
            }
            NewArrayInst inst2 = (NewArrayInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && inst.getType().equals(inst2.getType())
                    && comparator.usesEqual(inst.getDimensions(), inst2.getDimensions());
        }

        public Boolean visit(NewObjectInst inst, TACInst arg) {
            if (!(arg instanceof NewObjectInst)) {
                return Boolean.FALSE;
            }
            NewObjectInst inst2 = (NewObjectInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && inst.getType().equals(inst2.getType())
                    && comparator.usesEqual(inst.getArguments(), inst2.getArguments());
        }

        public Boolean visit(ReturnInst inst, TACInst arg) {
            if (!(arg instanceof ReturnInst)) {
                return Boolean.FALSE;
            }
            ReturnInst inst2 = (ReturnInst) arg;
            return comparator.useEqual(inst.getVar(), inst2.getVar());
        }

        public Boolean visit(SwitchInst inst, TACInst arg) {
            if (!(arg instanceof SwitchInst)) {
                return Boolean.FALSE;
            }
            SwitchInst inst2 = (SwitchInst) arg;
            return comparator.useEqual(inst.getIndex(), inst2.getIndex());
        }

        public Boolean visit(ThrowInst inst, TACInst arg) {
            if (!(arg instanceof ThrowInst)) {
                return Boolean.FALSE;
            }
            ThrowInst inst2 = (ThrowInst) arg;
            return comparator.useEqual(inst.getVar(), inst2.getVar());
        }

        public Boolean visit(UnaryInst inst, TACInst arg) {
            if (!(arg instanceof UnaryInst)) {
                return Boolean.FALSE;
            }
            UnaryInst inst2 = (UnaryInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && inst.getOperator() == inst2.getOperator()
                    && comparator.useEqual(inst.getVar(), inst2.getVar());
        }

        public Boolean visit(ChoiceInst inst, TACInst arg) {
            if (!(arg instanceof ChoiceInst)) {
                return Boolean.FALSE;
            }
            ChoiceInst inst2 = (ChoiceInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && comparator.usesEqual(inst.getChoices(), inst2.getChoices());
        }

        public Boolean visit(PhiInst inst, TACInst arg) {
            throw new ABCDException("Should not have Phi instruction during finally uninlining");
        }

        public Boolean visit(GetStaticFieldInst inst, TACInst arg) {
            if (!(arg instanceof GetStaticFieldInst)) {
                return Boolean.FALSE;
            }
            GetStaticFieldInst inst2 = (GetStaticFieldInst) arg;
            return comparator.defEqual(inst.getResult(), inst2.getResult())
                    && inst.getScope().equals(inst2.getScope())
                    && inst.getFieldName().equals(inst2.getFieldName());
        }

        public Boolean visit(SetStaticFieldInst inst, TACInst arg) {
            if (!(arg instanceof SetStaticFieldInst)) {
                return Boolean.FALSE;
            }
            SetStaticFieldInst inst2 = (SetStaticFieldInst) arg;
            return inst.getScope().equals(inst2.getScope())
                    && inst.getFieldName().equals(inst2.getFieldName())
                    && comparator.useEqual(inst.getValue(), inst2.getValue());
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

            TACInstSeq instructions = finallyExit.getInstructions();
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
                    for (Iterator<TACInst> it = bb.getInstructions().iterator();
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

            logger.log(Level.FINEST, "Try exits for finally subgraph (entry={0}) : {1}",
                    new Object[] {finallyEntry, tryExits});

            // uninline finally subgraph at try exits
            for (BasicBlock tryExit : tryExits) {
                Set<BasicBlock> toRemove = new HashSet<BasicBlock>();
                VariableComparator comparator = new VariableComparator();
                if (subgraphEquals(tryExit, finallyEntry, finallySubgraph,
                                   toRemove, comparator)) {
                    logger.log(Level.FINEST, "Remove finally subgraph : {0}",
                            toRemove);

//                        for (BasicBlock bb : toRemove) {
//                            CFG.removeBasicBlock(bb);
//                        }
                } else {
                    throw new ABCDException("Unable to uninline finally subgraph (entry=" +
                            finallyEntry + ") at try exit " + tryExit);
                }
            }
        }
    }

    private boolean subgraphEquals(BasicBlock block, BasicBlock otherBlock,
                                   DirectedGraph<BasicBlock, Edge> otherSubgraph,
                                   Set<BasicBlock> subgraph, VariableComparator comparator) {
        TACInstSeq seq1 = block.getInstructions();
        TACInstSeq seq2 = otherBlock.getInstructions();
        Boolean equals = seq1.accept(new TACComparator(seq2, comparator), null);
        if (Boolean.FALSE.equals(equals)) {
            return false;
        }

        subgraph.add(block);

        if (otherSubgraph.getSuccessorCountOf(otherBlock) == 0) {
            return true;
        }

        if (CFG.getSuccessorCountOf(block)
                != otherSubgraph.getSuccessorCountOf(otherBlock)) {
            return false;
        }

        for (Edge outgoingEdge : CFG.getOutgoingEdgesOf(block)) {
            BasicBlock successor = CFG.getEdgeTarget(outgoingEdge);
            boolean match = false;
            for (Edge otherOutgoingEdge : otherSubgraph.getOutgoingEdgesOf(otherBlock)) {
                if (Objects.equal(outgoingEdge.getValue(), otherOutgoingEdge.getValue())
                        && outgoingEdge.isLoopExit() == otherOutgoingEdge.isLoopExit()) {
                    BasicBlock otherSuccessor = otherSubgraph.getEdgeTarget(otherOutgoingEdge);
                    if (subgraphEquals(successor, otherSuccessor, otherSubgraph,
                                       subgraph, comparator)) {
                        match = true;
                        break;
                    }
                }
            }
            if (!match) {
                return false;
            }
        }

        return true;
    }
}
