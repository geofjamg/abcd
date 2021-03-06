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

import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import fr.jamgotchian.abcd.core.ast.expr.VariableExpression;
import fr.jamgotchian.abcd.core.ir.VariableFactory;
import java.util.Collections;
import java.util.Iterator;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BlockStatementTest {

    private BlockStatement block;

    private VariableFactory varFactory;

    @Before
    public void setup() {
         block = new BlockStatement();
         varFactory = new VariableFactory();
    }

    private Statement newStmt(int index) {
        VariableExpression varExpr = Expressions.newVarExpr(varFactory.create(index));
        return new LocalVariableDeclarationStatement(varExpr);
    }

    @Test
    public void emptyStatementTest() {
        Assert.assertTrue(block.isEmpty());
        Assert.assertTrue(block.getFirst() == null);
        Assert.assertTrue(block.getLast() == null);
        Assert.assertTrue(block.getNext() == null);
        Assert.assertTrue(block.getPrevious() == null);
        Assert.assertTrue(block.getBlock() == null);
        Assert.assertFalse(block.iterator().hasNext());
    }

    @Test
    public void addOneStatementToEmptyBlockTest() {
        Statement stmt = newStmt(0);
        block.add(stmt);
        Assert.assertFalse(block.isEmpty());
        Assert.assertTrue(block.getFirst() == stmt);
        Assert.assertTrue(block.getLast() == stmt);
        Assert.assertTrue(stmt.getBlock() == block);
        Assert.assertTrue(stmt.getPrevious() == null);
        Assert.assertTrue(stmt.getNext() == null);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt);
        Assert.assertFalse(it.hasNext());
    }

    @Test
    public void addTwoStatementToEmptyBlockTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        block.add(stmt0);
        block.add(stmt1);
        Assert.assertFalse(block.isEmpty());
        Assert.assertTrue(block.getFirst() == stmt0);
        Assert.assertTrue(block.getLast() == stmt1);
        Assert.assertTrue(stmt0.getPrevious() == null);
        Assert.assertTrue(stmt0.getNext() == stmt1);
        Assert.assertTrue(stmt1.getPrevious() == stmt0);
        Assert.assertTrue(stmt1.getNext() == null);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt0);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt1);
        Assert.assertFalse(it.hasNext());
    }

    @Test
    public void insertOneStatementToEmptyBlockTest() {
        Statement stmt = newStmt(0);
        block.insertAfter(null, stmt);
        Assert.assertFalse(block.isEmpty());
        Assert.assertTrue(block.getFirst() == stmt);
        Assert.assertTrue(block.getLast() == stmt);
        Assert.assertTrue(stmt.getBlock() == block);
        Assert.assertTrue(stmt.getPrevious() == null);
        Assert.assertTrue(stmt.getNext() == null);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt);
        Assert.assertFalse(it.hasNext());
    }

    @Test
    public void insertAtFirstStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        block.add(stmt0);
        block.add(stmt1);

        Statement stmt2 = newStmt(2);
        block.insertAfter(null, stmt2);

        Assert.assertFalse(block.isEmpty());
        Assert.assertTrue(block.getFirst() == stmt2);
        Assert.assertTrue(block.getLast() == stmt1);
        Assert.assertTrue(stmt0.getPrevious() == stmt2);
        Assert.assertTrue(stmt0.getNext() == stmt1);
        Assert.assertTrue(stmt1.getPrevious() == stmt0);
        Assert.assertTrue(stmt1.getNext() == null);
        Assert.assertTrue(stmt2.getPrevious() == null);
        Assert.assertTrue(stmt2.getNext() == stmt0);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt2);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt0);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt1);
        Assert.assertFalse(it.hasNext());
    }

    @Test
    public void insertAtMiddleStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        block.add(stmt0);
        block.add(stmt1);

        Statement stmt2 = newStmt(2);
        block.insertAfter(stmt0, stmt2);

        Assert.assertFalse(block.isEmpty());
        Assert.assertTrue(block.getFirst() == stmt0);
        Assert.assertTrue(block.getLast() == stmt1);
        Assert.assertTrue(stmt0.getPrevious() == null);
        Assert.assertTrue(stmt0.getNext() == stmt2);
        Assert.assertTrue(stmt1.getPrevious() == stmt2);
        Assert.assertTrue(stmt1.getNext() == null);
        Assert.assertTrue(stmt2.getPrevious() == stmt0);
        Assert.assertTrue(stmt2.getNext() == stmt1);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt0);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt2);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt1);
        Assert.assertFalse(it.hasNext());
    }

    @Test
    public void insertAtLastStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        block.add(stmt0);
        block.add(stmt1);

        Statement stmt2 = newStmt(2);
        block.insertAfter(stmt1, stmt2);

        Assert.assertFalse(block.isEmpty());
        Assert.assertTrue(block.getFirst() == stmt0);
        Assert.assertTrue(block.getLast() == stmt2);
        Assert.assertTrue(stmt0.getPrevious() == null);
        Assert.assertTrue(stmt0.getNext() == stmt1);
        Assert.assertTrue(stmt1.getPrevious() == stmt0);
        Assert.assertTrue(stmt1.getNext() == stmt2);
        Assert.assertTrue(stmt2.getPrevious() == stmt1);
        Assert.assertTrue(stmt2.getNext() == null);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt0);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt1);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt2);
        Assert.assertFalse(it.hasNext());
    }

    @Test
    public void removeFreeStatementTest() {
        Statement stmt = newStmt(0);
        try {
            stmt.remove();
            Assert.assertFalse(true);
        } catch(Throwable t) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void removeMiddleStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        Statement stmt2 = newStmt(2);
        block.add(stmt0);
        block.add(stmt1);
        block.add(stmt2);

        stmt1.remove();

        Assert.assertTrue(stmt1.getBlock() == null);
        Assert.assertTrue(stmt1.getPrevious() == null);
        Assert.assertTrue(stmt1.getNext() == null);
        Assert.assertTrue(stmt0.getNext() == stmt2);
        Assert.assertTrue(stmt2.getPrevious() == stmt0);
        Assert.assertTrue(block.getFirst() == stmt0);
        Assert.assertTrue(block.getLast() == stmt2);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt0);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt2);
        Assert.assertFalse(it.hasNext());
    }

    @Test
    public void removeFirstStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        Statement stmt2 = newStmt(2);
        block.add(stmt0);
        block.add(stmt1);
        block.add(stmt2);

        stmt0.remove();

        Assert.assertTrue(stmt0.getBlock() == null);
        Assert.assertTrue(stmt0.getPrevious() == null);
        Assert.assertTrue(stmt0.getNext() == null);
        Assert.assertTrue(stmt1.getPrevious() == null);
        Assert.assertTrue(stmt1.getNext() == stmt2);
        Assert.assertTrue(stmt2.getPrevious() == stmt1);
        Assert.assertTrue(stmt2.getNext() == null);
        Assert.assertTrue(block.getFirst() == stmt1);
        Assert.assertTrue(block.getLast() == stmt2);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt1);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt2);
        Assert.assertFalse(it.hasNext());
    }


    @Test
    public void removeLastStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        Statement stmt2 = newStmt(2);
        block.add(stmt0);
        block.add(stmt1);
        block.add(stmt2);

        stmt2.remove();

        Assert.assertTrue(stmt2.getBlock() == null);
        Assert.assertTrue(stmt2.getPrevious() == null);
        Assert.assertTrue(stmt2.getNext() == null);

        Assert.assertTrue(stmt0.getPrevious() == null);
        Assert.assertTrue(stmt0.getNext() == stmt1);
        Assert.assertTrue(stmt1.getPrevious() == stmt0);
        Assert.assertTrue(stmt1.getNext() == null);
        Assert.assertTrue(block.getFirst() == stmt0);
        Assert.assertTrue(block.getLast() == stmt1);
        Iterator<Statement> it = block.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt0);
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt1);
        Assert.assertFalse(it.hasNext());
    }

    @Test
    public void addAlreadyUsedStatementTest() {
        BlockStatement otherBlock = new BlockStatement();
        Statement stmt = newStmt(0);
        otherBlock.add(stmt);
        block.add(stmt);
        Assert.assertTrue(otherBlock.isEmpty());
        Assert.assertFalse(block.isEmpty());
        Assert.assertTrue(stmt.getBlock() == block);
    }


    @Test
    public void replaceMiddleStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        Statement stmt2 = newStmt(2);
        block.add(stmt0);
        block.add(stmt1);
        block.add(stmt2);

        Statement stmt3 = newStmt(3);
        block.replace(stmt1, Collections.singletonList(stmt3));

        Assert.assertTrue(stmt3.getBlock() == block);
        Assert.assertTrue(stmt3.getPrevious() == stmt0);
        Assert.assertTrue(stmt3.getNext() == stmt2);
        Assert.assertTrue(stmt0.getNext() == stmt3);
        Assert.assertTrue(stmt2.getPrevious() == stmt3);
        Assert.assertTrue(block.getFirst() == stmt0);
        Assert.assertTrue(block.getLast() == stmt2);
    }

    @Test
    public void replaceFirstStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        Statement stmt2 = newStmt(2);
        block.add(stmt0);
        block.add(stmt1);
        block.add(stmt2);

        Statement stmt3 = newStmt(3);
        block.replace(stmt0, Collections.singletonList(stmt3));

        Assert.assertTrue(stmt3.getBlock() == block);
        Assert.assertTrue(stmt3.getPrevious() == null);
        Assert.assertTrue(stmt3.getNext() == stmt1);
        Assert.assertTrue(stmt1.getPrevious() == stmt3);
        Assert.assertTrue(stmt1.getNext() == stmt2);
        Assert.assertTrue(stmt2.getPrevious() == stmt1);
        Assert.assertTrue(stmt2.getNext() == null);
        Assert.assertTrue(block.getFirst() == stmt3);
        Assert.assertTrue(block.getLast() == stmt2);
    }


    @Test
    public void replaceLastStatementTest() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        Statement stmt2 = newStmt(2);
        block.add(stmt0);
        block.add(stmt1);
        block.add(stmt2);

        Statement stmt3 = newStmt(3);
        block.replace(stmt2, Collections.singletonList(stmt3));

        Assert.assertTrue(stmt3.getBlock() == block);
        Assert.assertTrue(stmt3.getPrevious() == stmt1);
        Assert.assertTrue(stmt3.getNext() == null);
        Assert.assertTrue(stmt0.getPrevious() == null);
        Assert.assertTrue(stmt0.getNext() == stmt1);
        Assert.assertTrue(stmt1.getPrevious() == stmt0);
        Assert.assertTrue(stmt1.getNext() == stmt3);
        Assert.assertTrue(block.getFirst() == stmt0);
        Assert.assertTrue(block.getLast() == stmt3);
    }

    @Test
    public void testClear() {
        Statement stmt0 = newStmt(0);
        Statement stmt1 = newStmt(1);
        Statement stmt2 = newStmt(2);
        block.add(stmt0);
        block.add(stmt1);
        block.add(stmt2);
        block.clear();
        Assert.assertTrue(block.isEmpty());
        Assert.assertTrue(block.getFirst() == null);
        Assert.assertTrue(block.getLast() == null);
    }
}
