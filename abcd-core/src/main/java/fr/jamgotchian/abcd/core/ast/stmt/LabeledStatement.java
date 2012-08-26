/*
 *  Copyright (C) 2011 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LabeledStatement extends AbstractStatement {

    private final String label;

    private final Statement stmt;

    public LabeledStatement(String label, Statement stmt) {
        if (label == null) {
            throw new IllegalArgumentException("label == null");
        }
        if (stmt == null) {
            throw new IllegalArgumentException("stmt == null");
        }
        this.label = label;
        this.stmt = stmt;
    }

    public String getLabel() {
        return label;
    }

    public Statement getStmt() {
        return stmt;
    }

    @Override
    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
