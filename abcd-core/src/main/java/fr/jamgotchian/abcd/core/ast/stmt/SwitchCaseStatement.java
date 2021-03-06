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

package fr.jamgotchian.abcd.core.ast.stmt;

import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ir.CaseValues;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SwitchCaseStatement extends AbstractStatement {

    public static class CaseStatement {

        private final CaseValues values;

        private final List<Statement> stmts;

        public CaseStatement(CaseValues values, List<Statement> stmts) {
            if (values == null) {
                throw new IllegalArgumentException("values == null");
            }
            if (stmts == null) {
                throw new IllegalArgumentException("stmts == null");
            }
            this.values = values;
            this.stmts = stmts;
        }

        public CaseStatement(CaseValues values) {
            this(values, new ArrayList<Statement>());
        }

        public CaseValues getValues() {
            return values;
        }

        public List<Statement> getStmts() {
            return stmts;
        }
    }

    private final Expression index;

    private final List<CaseStatement> cases;

    public SwitchCaseStatement(Expression index, List<CaseStatement> cases) {
        if (index == null) {
            throw new IllegalArgumentException("index == null");
        }
        if (cases == null) {
            throw new IllegalArgumentException("cases == null");
        }
        this.index = index;
        this.cases = cases;
    }

    public SwitchCaseStatement(Expression condition) {
        this(condition, new ArrayList<CaseStatement>(1));
    }

    @Override
    public void setBlock(BlockStatement block) {
        for (CaseStatement _case : cases) {
            for (Statement stmt : _case.getStmts()) {
                stmt.setBlock(block);
            }
        }
        super.setBlock(block);
    }

    public Expression getIndex() {
        return index;
    }

    public List<CaseStatement> getCases() {
        return cases;
    }

    public void addCase(CaseStatement _case) {
        cases.add(_case);
    }

    @Override
    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
