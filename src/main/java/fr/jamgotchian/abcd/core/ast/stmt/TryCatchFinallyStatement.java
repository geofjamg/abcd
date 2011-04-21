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

import java.util.Collection;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TryCatchFinallyStatement extends AbstractStatement {

    private final BlockStatement _try;

    public static class CatchClause {
        
        private final BlockStatement blockStmt;
    
        private final LocalVariableDeclaration exceptionVarDecl;

        public CatchClause(BlockStatement blockStmt, LocalVariableDeclaration exceptionVarDecl) {
            if (blockStmt == null) {
                throw new IllegalArgumentException("blockStmt == null");
            }
            if (exceptionVarDecl == null) {
                throw new IllegalArgumentException("exceptionVarDecl == null");
            }
            this.blockStmt = blockStmt;
            this.exceptionVarDecl = exceptionVarDecl;
        }

        public BlockStatement getBlockStmt() {
            return blockStmt;
        }

        public LocalVariableDeclaration getExceptionVarDecl() {
            return exceptionVarDecl;
        }
    }
    
    private final Collection<CatchClause> catchs;

    private BlockStatement _finally;
    
    public TryCatchFinallyStatement(BlockStatement _try, Collection<CatchClause> catchs) {
        if (_try == null) {
            throw new IllegalArgumentException("_try == null");
        }
        if (catchs == null) {
            throw new IllegalArgumentException("catchs == null");
        }
        if (catchs.isEmpty()) {
            throw new IllegalArgumentException("catchs.isEmpty()");
        }
        this._try = _try;
        this.catchs = catchs;
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

    public void setFinally(BlockStatement _finally) {
        this._finally = _finally;
        _finally.setBlock(getBlock());
    }

    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

}
