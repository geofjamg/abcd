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
import org.jf.dexlib.Code.Format.Instruction11x;
import org.jf.dexlib.Code.Format.Instruction12x;
import org.jf.dexlib.Code.Format.Instruction21c;
import org.jf.dexlib.Code.Format.Instruction21h;
import org.jf.dexlib.Code.Format.Instruction21s;
import org.jf.dexlib.Code.Format.Instruction21t;
import org.jf.dexlib.Code.Format.Instruction22b;
import org.jf.dexlib.Code.Format.Instruction22c;
import org.jf.dexlib.Code.Format.Instruction22t;
import org.jf.dexlib.Code.Format.Instruction23x;
import org.jf.dexlib.Code.Format.Instruction30t;
import org.jf.dexlib.Code.Format.Instruction31i;
import org.jf.dexlib.Code.Format.Instruction31t;
import org.jf.dexlib.Code.Format.Instruction35c;
import org.jf.dexlib.Code.Format.Instruction51l;
import org.jf.dexlib.Code.Format.PackedSwitchDataPseudoInstruction;
import org.jf.dexlib.Code.Instruction;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class DalvikBytecodeVisitor {

    public abstract void before(BasicBlock bb);

    public abstract void visit(BasicBlock bb, int position, Instruction10t inst, CodeAddressManager addressManager);

    public abstract void visit(BasicBlock bb, int position, Instruction10x inst);

    public abstract void visit(BasicBlock bb, int position, Instruction11n inst);

    public abstract void visit(BasicBlock bb, int position, Instruction11x inst);

    public abstract void visit(BasicBlock bb, int position, Instruction12x inst);

    public abstract void visit(BasicBlock bb, int position, Instruction21c inst);

    public abstract void visit(BasicBlock bb, int position, Instruction21h inst);

    public abstract void visit(BasicBlock bb, int position, Instruction21s inst);

    public abstract void visit(BasicBlock bb, int position, Instruction21t inst, CodeAddressManager addressManager);

    public abstract void visit(BasicBlock bb, int position, Instruction22b inst);

    public abstract void visit(BasicBlock bb, int position, Instruction22c inst);

    public abstract void visit(BasicBlock bb, int position, Instruction22t inst, CodeAddressManager addressManager);

    public abstract void visit(BasicBlock bb, int position, Instruction23x inst);

    public abstract void visit(BasicBlock bb, int position, Instruction30t inst, CodeAddressManager addressManager);

    public abstract void visit(BasicBlock bb, int position, Instruction31i inst);

    public abstract void visit(BasicBlock bb, int position, Instruction31t inst, CodeAddressManager addressManager);

    public abstract void visit(BasicBlock bb, int position, Instruction35c inst);

    public abstract void visit(BasicBlock bb, int position, Instruction51l inst);

    public abstract void visit(BasicBlock bb, int position, PackedSwitchDataPseudoInstruction inst, CodeAddressManager addressManager);

    public abstract void visitTODO(BasicBlock bb, int position, Instruction inst);

    public abstract void after(BasicBlock bb);

    public void visit(Instruction[] instructions, BasicBlock bb, CodeAddressManager addressManager) {

        before(bb);

        Range range = bb.getRange();
        if (range != null) {
            for (int position : range) {
                Instruction inst = instructions[position];
                switch(inst.getFormat()) {
                    case Format10t:
                        visit(bb, position, (Instruction10t) inst, addressManager);
                        break;

                    case Format10x:
                        visit(bb, position, (Instruction10x) inst);
                        break;

                    case Format11n:
                        visit(bb, position, (Instruction11n) inst);
                        break;

                    case Format11x:
                        visit(bb, position, (Instruction11x) inst);
                        break;

                    case Format12x:
                        visit(bb, position, (Instruction12x) inst);
                        break;

                    case Format21c:
                        visit(bb, position, (Instruction21c) inst);
                        break;

                    case Format21h:
                        visit(bb, position, (Instruction21h) inst);
                        break;

                    case Format21s:
                        visit(bb, position, (Instruction21s) inst);
                        break;

                    case Format21t:
                        visit(bb, position, (Instruction21t) inst, addressManager);
                        break;

                    case Format22b:
                        visit(bb, position, (Instruction22b) inst);
                        break;

                    case Format22c:
                        visit(bb, position, (Instruction22c) inst);
                        break;

                    case Format22t:
                        visit(bb, position, (Instruction22t) inst, addressManager);
                        break;

                    case Format23x:
                        visit(bb, position, (Instruction23x) inst);
                        break;

                    case Format30t:
                        visit(bb, position, (Instruction30t) inst, addressManager);
                        break;

                    case Format31i:
                        visit(bb, position, (Instruction31i) inst);
                        break;

                    case Format31t:
                        visit(bb, position, (Instruction31t) inst, addressManager);
                        break;

                    case Format35c:
                        visit(bb, position, (Instruction35c) inst);
                        break;

                    case Format51l:
                        visit(bb, position, (Instruction51l) inst);
                        break;

                    case PackedSwitchData:
                        visit(bb, position, (PackedSwitchDataPseudoInstruction) inst, addressManager);
                        break;

                    default:
                        visitTODO(bb, position, inst);
                }
            }
        }

        after(bb);
    }
}
