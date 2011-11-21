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
package fr.jamgotchian.abcd.core.bytecode.dalvik;

import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.util.Range;
import org.jf.dexlib.Code.Format.Instruction10t;
import org.jf.dexlib.Code.Format.Instruction10x;
import org.jf.dexlib.Code.Format.Instruction11n;
import org.jf.dexlib.Code.Format.Instruction21c;
import org.jf.dexlib.Code.Format.Instruction22t;
import org.jf.dexlib.Code.Format.Instruction35c;
import org.jf.dexlib.Code.Instruction;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class DalvikBytecodeVisitor {

    public abstract void before(BasicBlock bb);

    public abstract void visit(BasicBlock bb, int position, Instruction10t inst);

    public abstract void visit(BasicBlock bb, int position, Instruction11n inst);

    public abstract void visit(BasicBlock bb, int position, Instruction21c inst);

    public abstract void visit(BasicBlock bb, int position, Instruction22t inst);

    public abstract void visit(BasicBlock bb, int position, Instruction35c inst);

    public abstract void visit(BasicBlock bb, int position, Instruction10x inst);

    public abstract void visitTODO(BasicBlock bb, int position, Instruction inst);

    public abstract void after(BasicBlock bb);

    public void visit(Instruction[] instructions, BasicBlock bb) {

        before(bb);

        Range range = bb.getRange();
        if (range != null) {
            for (int position : range) {
                Instruction inst = instructions[position];
                switch(inst.getFormat()) {
                    case Format10t:
                        visit(bb, position, (Instruction10t) inst);
                        break;

                    case Format10x:
                        visit(bb, position, (Instruction10x) inst);
                        break;

                    case Format11n:
                        visit(bb, position, (Instruction11n) inst);
                        break;

                    case Format21c:
                        visit(bb, position, (Instruction21c) inst);
                        break;

                    case Format22t:
                        visit(bb, position, (Instruction22t) inst);
                        break;

                    case Format35c:
                        visit(bb, position, (Instruction35c) inst);
                        break;

                    default:
                        visitTODO(bb, position, inst);
                }
            }
        }

        after(bb);
    }
}
