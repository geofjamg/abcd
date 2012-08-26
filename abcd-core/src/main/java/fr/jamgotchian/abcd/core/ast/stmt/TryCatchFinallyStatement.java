/*
 *  Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.ast.stmt;

import fr.jamgotchian.abcd.core.ast.expr.VariableExpression;
import java.util.Collection;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TryCatchFinallyStatement extends AbstractStatement {

    private final BlockStatement _try;

    public static class CatchClause {

        private final BlockStatement blockStmt;

        private final VariableExpression exceptionVar;

        public CatchClause(BlockStatement blockStmt, VariableExpression exceptionVar) {
            if (blockStmt == null) {
                throw new IllegalArgumentException("blockStmt == null");
            }
            if (exceptionVar == null) {
                throw new IllegalArgumentException("exceptionVar == null");
            }
            this.blockStmt = blockStmt;
            this.exceptionVar = exceptionVar;
        }

        public BlockStatement getBlockStmt() {
            return blockStmt;
        }

        public VariableExpression getExceptionVar() {
            return exceptionVar;
        }
    }

    private final Collection<CatchClause> catchs;

    private final BlockStatement _finally;

    public TryCatchFinallyStatement(BlockStatement _try, Collection<CatchClause> catchs,
                                    BlockStatement _finally) {
        if (_try == null) {
            throw new IllegalArgumentException("_try == null");
        }
        if (catchs == null) {
            throw new IllegalArgumentException("catchs == null");
        }
        this._try = _try;
        this.catchs = catchs;
        this._finally = _finally;
    }

    @Override
    public void setBlock(BlockStatement block) {
        _try.setBlock(block);
        for (CatchClause _catch : catchs) {
            _catch.getBlockStmt().setBlock(block);
        }
        if (_finally != null) {
            _finally.setBlock(block);
        }
        super.setBlock(block);
    }

    public BlockStatement getTry() {
        return _try;
    }

    public Collection<CatchClause> getCatchs() {
        return catchs;
    }

    public BlockStatement getFinally() {
        return _finally;
    }

    @Override
    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

}
