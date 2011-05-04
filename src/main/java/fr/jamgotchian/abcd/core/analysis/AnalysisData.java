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

import fr.jamgotchian.abcd.core.ast.util.ExpressionStack;
import fr.jamgotchian.abcd.core.ast.stmt.GotoStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LabelStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockAnalysisData;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.TemporaryVariable;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class AnalysisData implements BasicBlockAnalysisData {

    private final List<Statement> statements;

    private ExpressionStack outputStack;

    private ExpressionStack inputStack;

    private int stackConsumption;

    private int stackProduction;

    private ArrayDeque<TemporaryVariable> inputStack2;

    private ArrayDeque<TemporaryVariable> outputStack2;

    private List<TACInst> instructions;

    public AnalysisData() {
        this.statements = new ArrayList<Statement>();
        stackConsumption = 0;
        stackProduction = 0;
        instructions = new ArrayList<TACInst>();
    }

    public int getStatementCount() {
        return getUsefullStatements().size();
    }

    public Collection<Statement> getStatements() {
        return statements;
    }

    public Collection<Statement> getUsefullStatements() {
        List<Statement> usefullStmts = new ArrayList<Statement>();
        for (Statement stmt : statements) {
            if (stmt instanceof LabelStatement || stmt instanceof GotoStatement) {
                continue;
            }
            usefullStmts.add(stmt);
        }
        return usefullStmts;
    }

    public void addStatement(Statement stmt) {
        statements.add(stmt);
    }

    public Statement getLastStatement() {
        if (statements.isEmpty()) {
            return null;
        } else {
            return statements.get(statements.size()-1);
        }
    }

    public ExpressionStack getOutputStack() {
        return outputStack;
    }

    public void setOutputStack(ExpressionStack stack) {
        outputStack = stack;
    }

    public ExpressionStack getInputStack() {
        return inputStack;
    }

    public void setInputStack(ExpressionStack stack) {
        inputStack = stack;
    }

    public int getStackConsumption() {
        return stackConsumption;
    }

    public void setStackConsumption(int stackConsumption) {
        this.stackConsumption = stackConsumption;
    }

    public int getStackProduction() {
        return stackProduction;
    }

    public void setStackProduction(int stackProduction) {
        this.stackProduction = stackProduction;
    }

    public List<TACInst> getInstructions() {
        return instructions;
    }

    public TACInst getLastInst() {
        return instructions.isEmpty() ? null : instructions.get(instructions.size()-1);
    }

    public ArrayDeque<TemporaryVariable> getInputStack2() {
        return inputStack2;
    }

    public void setInputStack2(ArrayDeque<TemporaryVariable> inputStack2) {
        this.inputStack2 = inputStack2;
    }

    public ArrayDeque<TemporaryVariable> getOutputStack2() {
        return outputStack2;
    }

    public void setOutputStack2(ArrayDeque<TemporaryVariable> outputStack2) {
        this.outputStack2 = outputStack2;
    }
}
