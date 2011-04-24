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

import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.Iterator;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class StatementsTest {

    private BlockStatement sourceBlock;
    private BlockStatement targetBlock;

    @Before
    public void setup() {
         sourceBlock = new BlockStatement();
         targetBlock = new BlockStatement();
    }

    @Test
    public void emptySourceBlockMoveTest() {
        Assert.assertTrue(sourceBlock.isEmpty());
        Assert.assertTrue(targetBlock.isEmpty());
        Statements.move(sourceBlock.getFirst(), targetBlock.getLast(), targetBlock);
        Assert.assertTrue(sourceBlock.isEmpty());
        Assert.assertTrue(targetBlock.isEmpty());
    }

    @Test
    public void oneStatementSourceBlockMoveTest() {
        Statement stmt = new LocalVariableDeclarationStatement(0, JavaType.INT);
        sourceBlock.add(stmt);
        Assert.assertFalse(sourceBlock.isEmpty());
        Assert.assertTrue(targetBlock.isEmpty());
        Statements.move(sourceBlock.getFirst(), targetBlock.getLast(), targetBlock);
        Assert.assertTrue(sourceBlock.isEmpty());
        Assert.assertFalse(targetBlock.isEmpty());
        Assert.assertTrue(stmt.getBlock() == targetBlock);
        Iterator<Statement> it = targetBlock.iterator();
        Assert.assertTrue(it.hasNext());
        Assert.assertTrue(it.next() == stmt);
        Assert.assertFalse(it.hasNext());
    }
}
