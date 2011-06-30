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

import fr.jamgotchian.abcd.core.controlflow.NameGenerator;
import fr.jamgotchian.abcd.core.controlflow.SimpleNameGenerator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BlockStatement extends AbstractStatement
                              implements Iterable<Statement> {

    private static volatile int ID_GENERATOR = 0;

    private int id;

    private Statement first;

    private Statement last;

    private final LocalVariableTable localVariableTable;

    private final NameGenerator nameGenerator;

    public BlockStatement() {
        id = ID_GENERATOR++;
        localVariableTable = new LocalVariableTableImpl();
        nameGenerator = new SimpleNameGenerator();
    }

    public BlockStatement(Iterable<Statement> stmts) {
        this();
        addAll(stmts);
    }

    public Statement getFirst() {
        return first;
    }

    public void setFirst(Statement first) {
        this.first = first;
    }

    public Statement getLast() {
        return last;
    }

    public void setLast(Statement last) {
        this.last = last;
    }

    public LocalVariableTable getLocalVariableTable() {
        return localVariableTable;
    }

    public NameGenerator getNameGenerator() {
        return nameGenerator;
    }

    public boolean isEmpty() {
        return first == null;
    }

    public Iterator<Statement> iterator() {
        return new Iterator<Statement>() {

            private Statement stmt;

            public boolean hasNext() {
                if (stmt == null) {
                    return first != null;
                } else {
                    return stmt.getNext() != null;
                }
            }

            public Statement next() {
                if (stmt == null) {
                    stmt = first;
                } else {
                    stmt = stmt.getNext();
                }
                return stmt;
            }

            public void remove() {
                if (stmt != null) {
                    stmt.remove();
                }
            }
        };
    }

    public void insertAfter(Statement stmt, Statement newStmt) {
        if (stmt != null) {
            if (stmt.getBlock() != this) {
                throw new IllegalArgumentException("baseStmt.getBlock() != this");
            }
        }

        newStmt.remove();

        if (first == null) {
            first = newStmt;
            last = newStmt;
        } else {
            if (stmt == null) {
                first.setPrevious(newStmt);
                newStmt.setNext(first);
                first = newStmt;
            } else {
                if (stmt == last) {
                    last = newStmt;
                } else {
                    stmt.getNext().setPrevious(newStmt);
                    newStmt.setNext(stmt.getNext());
                }
                newStmt.setPrevious(stmt);
                stmt.setNext(newStmt);
            }
        }

        newStmt.setBlock(this);
    }

    public void insertBefore(Statement stmt, Statement newStmt) {
        insertAfter(stmt.getPrevious(), newStmt);
    }

    public void add(Statement newStmt) {
        insertAfter(last, newStmt);
    }

    final public void addAll(Iterable<Statement> stmts) {
        for (Statement stmt : stmts) {
            add(stmt);
        }
    }

    public void clear() {
        while (first != null) {
            remove(first);
        }
    }

    public void remove(Statement stmt) {
        if (stmt.getBlock() != this) {
            throw new IllegalArgumentException("stmt.getBlock() != this");
        }
        if (stmt.getPrevious() != null) {
            stmt.getPrevious().setNext(stmt.getNext());
        } else {
            first = stmt.getNext();
        }
        if (stmt.getNext() != null) {
            stmt.getNext().setPrevious(stmt.getPrevious());
        } else {
            last = stmt.getPrevious();
        }
        stmt.setPrevious(null);
        stmt.setNext(null);
        stmt.setBlock(null);
    }

    public void replace(Statement oldStmt, Collection<Statement> newStmts) {
        if (oldStmt.getBlock() != this) {
            throw new IllegalArgumentException("oldStmt.getBlock() != this");
        }

        Statement prevStmt = oldStmt.getPrevious();

        remove(oldStmt);

        for (Statement newStmt : newStmts) {
            insertAfter(prevStmt, newStmt);
            prevStmt = newStmt;
        }
    }

    public int getDepth() {
        int depth = 0;
        for (BlockStatement b = getBlock(); b != null; b = b.getBlock()) {
            depth++;
        }
        return depth;
    }

    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

    private String findVariable(int index) {
        String name = localVariableTable.getVariable(index);
        if (name != null) {
            return name;
        } else {
            if (getBlock() != null) {
                return getBlock().findVariable(index);
            } else {
                return null;
            }
        }
    }

    private boolean containsVariable(String name) {
        if (localVariableTable.containsVariable(name)) {
            return true;
        } else {
            if (getBlock() != null) {
                return getBlock().containsVariable(name);
            } else {
                return false;
            }
        }
    }

    public String getVariable(int index) {
        String name = findVariable(index);
        if (name == null) {
            do {
                name = nameGenerator.generate();
            } while (containsVariable(name));

            localVariableTable.addVariable(index, name);
        }
        return name;
    }

    public List<String> getDebugInfos() {
        List<Integer> parentIds = new ArrayList<Integer>();
        for (BlockStatement b = getBlock(); b != null; b = b.getBlock()) {
            parentIds.add(b.id);
        }
        List<String> infos = new ArrayList<String>(3);
        infos.add("block id    : " + id);
        infos.add("block depth : " + getDepth());
        infos.add("parents id  : " + parentIds);
        infos.add("variables   : " + localVariableTable.getVariables());
        return infos;
    }
}
