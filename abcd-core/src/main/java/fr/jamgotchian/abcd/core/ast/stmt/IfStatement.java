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

import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.util.ExpressionInverter;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IfStatement extends AbstractStatement {

    private Expression condition;

    private BlockStatement then;

    private BlockStatement _else;

    public IfStatement(Expression condition, BlockStatement then, BlockStatement _else) {
        if (condition == null) {
            throw new IllegalArgumentException("condition == null");
        }
        this.condition = condition;
        this.then = then;
        this._else = _else;
    }

    public IfStatement(Expression condition) {
        this(condition, null, null);
    }

    @Override
    public void setBlock(BlockStatement block) {
        if (then != null) {
            then.setBlock(block);
        }
        if (_else != null) {
            _else.setBlock(block);
        }
        super.setBlock(block);
    }

    public Expression getCondition() {
        return condition;
    }

    public void invertCondition() {
        condition = ExpressionInverter.invert(condition);
    }

    public BlockStatement getThen() {
        return then;
    }

    public BlockStatement getElse() {
        return _else;
    }

    public void setThen(BlockStatement then) {
        this.then = then;
        then.setBlock(getBlock());
    }

    public void setElse(BlockStatement _else) {
        this._else = _else;
        _else.setBlock(getBlock());
    }

    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

}
