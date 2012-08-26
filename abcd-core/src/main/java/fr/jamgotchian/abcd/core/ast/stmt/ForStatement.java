/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ForStatement extends AbstractStatement {

    private final Expression init;

    private final Expression condition;

    private final Expression update;

    private final BlockStatement body;

    public ForStatement(Expression init, Expression condition, Expression update,
                          BlockStatement body) {
        if (init == null) {
            throw new IllegalArgumentException("init == null");
        }
        if (condition == null) {
            throw new IllegalArgumentException("condition == null");
        }
        if (update == null) {
            throw new IllegalArgumentException("update == null");
        }
        this.init = init;
        this.condition = condition;
        this.update = update;
        this.body = body;
    }

    public ForStatement(Expression init, Expression condition, Expression update) {
        this(init, condition, update, new BlockStatement());
    }

    public Expression getInit() {
        return init;
    }

    public Expression getCondition() {
        return condition;
    }

    public Expression getUpdate() {
        return update;
    }

    public BlockStatement getBody() {
        return body;
    }

    @Override
    public void setBlock(BlockStatement block) {
        super.setBlock(block);
        body.setBlock(block);
    }

    @Override
    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

}
