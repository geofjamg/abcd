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

import fr.jamgotchian.abcd.core.common.Label;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LookupOrTableSwitchStatement extends AbstractStatement {

    private final Expression condition;

    private final List<Label> labels;

    public LookupOrTableSwitchStatement(Expression condition, List<Label> labels) {
        if (condition == null) {
            throw new IllegalArgumentException("condition == null");
        }
        if (labels == null) {
            throw new IllegalArgumentException("labels == null");
        }
        if (labels.isEmpty()) {
            throw new IllegalArgumentException("labels.isEmpty()");
        }
        this.condition = condition;
        this.labels = labels;
    }

    public Expression getCondition() {
        return condition;
    }

    public List<Label> getLabels() {
        return Collections.unmodifiableList(labels);
    }

    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

}
