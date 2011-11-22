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

import fr.jamgotchian.abcd.core.code.CodeWriter;
import fr.jamgotchian.abcd.core.code.DOTHTMLLikeCodeWriter;
import fr.jamgotchian.abcd.core.code.TextCodeWriter;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.BasicBlockImpl;
import fr.jamgotchian.abcd.core.ir.BasicBlockType;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.List;
import org.jf.dexlib.Code.Format.Instruction10t;
import org.jf.dexlib.Code.Format.Instruction10x;
import org.jf.dexlib.Code.Format.Instruction11n;
import org.jf.dexlib.Code.Format.Instruction21c;
import org.jf.dexlib.Code.Format.Instruction21t;
import org.jf.dexlib.Code.Format.Instruction22t;
import org.jf.dexlib.Code.Format.Instruction35c;
import org.jf.dexlib.Code.Instruction;
import org.jf.dexlib.MethodIdItem;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DalvikBytecodeWriter extends DalvikBytecodeVisitor {

    public static String toText(Instruction[] instructions, CodeAddressManager addressManager) {
        StringWriter writer = new StringWriter();
        BasicBlock bb = new BasicBlockImpl(0, instructions.length-1, null);
        new DalvikBytecodeWriter(new TextCodeWriter(writer))
                .visit(instructions, bb, addressManager);
        return writer.toString();
    }

    public static String toDOTHTMLLike(Instruction[] instructions, BasicBlock bb, CodeAddressManager addressManager) {
        StringWriter writer = new StringWriter();
        new DalvikBytecodeWriter(new DOTHTMLLikeCodeWriter(writer)).visit(instructions, bb, addressManager);
        return writer.toString();
    }

    private final CodeWriter writer;

    public DalvikBytecodeWriter(CodeWriter writer) {
        this.writer = writer;
    }

    @Override
    public void before(BasicBlock bb) {
        writer.before(null);
        if (bb.getType() == BasicBlockType.ENTRY
                || bb.getType() == BasicBlockType.EXIT) {
            writer.write(bb.getType());
        }
    }

    @Override
    public void visit(BasicBlock bb, int position, Instruction10t inst,
                      CodeAddressManager addressManager) {
        int targetPosition
                = addressManager.getTargetPosition(position,
                                                   inst.getTargetAddressOffset());
        writer.writeIndex(position)
                .writeKeyword(inst.opcode.toString().toLowerCase())
                .writeSpace().write(targetPosition)
                .newLine();
    }

    @Override
    public void visit(BasicBlock bb, int position, Instruction10x inst) {
        writer.writeIndex(position)
                .writeKeyword(inst.opcode.toString().toLowerCase())
                .newLine();
    }

    @Override
    public void visit(BasicBlock bb, int position, Instruction11n inst) {
        writer.writeIndex(position)
                .writeKeyword(inst.opcode.toString().toLowerCase())
                .writeSpace().write("r").write(inst.getRegisterA())
                .writeSpace().write(inst.getLiteral())
                .newLine();
    }

    @Override
    public void visit(BasicBlock bb, int position, Instruction21c inst) {
        writer.writeIndex(position)
                .writeKeyword(inst.opcode.toString().toLowerCase())
                .writeSpace().write("r").write(inst.getRegisterA())
                .writeSpace().write(inst.getReferencedItem())
                .newLine();
    }

    @Override
    public void visit(BasicBlock bb, int position, Instruction21t inst,
                      CodeAddressManager addressManager) {
        int targetPosition
                = addressManager.getTargetPosition(position,
                                                   inst.getTargetAddressOffset());
        writer.writeIndex(position)
                .writeKeyword(inst.opcode.toString().toLowerCase())
                .writeSpace().write("r").write(inst.getRegisterA())
                .writeSpace().write(targetPosition)
                .newLine();
    }

    @Override
    public void visit(BasicBlock bb, int position, Instruction22t inst,
                      CodeAddressManager addressManager) {
        int targetPosition
                = addressManager.getTargetPosition(position,
                                                   inst.getTargetAddressOffset());
        writer.writeIndex(position)
                .writeKeyword(inst.opcode.toString().toLowerCase())
                .writeSpace().write("r").write(inst.getRegisterA())
                .writeSpace().write("r").write(inst.getRegisterB())
                .writeSpace().write(targetPosition)
                .newLine();
    }

    @Override
    public void visit(BasicBlock bb, int position, Instruction35c inst) {
        writer.writeIndex(position)
                .writeKeyword(inst.opcode.toString().toLowerCase());
        MethodIdItem methodId = (MethodIdItem) inst.getReferencedItem();
        writer.writeSpace().write(writer.removeSpecialCharacters(methodId.getMethodString().toString()));
        List<String> registers
                = Arrays.asList("r" + inst.getRegisterA(), "r" + inst.getRegisterD(),
                                "r" + inst.getRegisterE(), "r" + inst.getRegisterF(),
                                "r" + inst.getRegisterG());
        writer.writeSpace().write(registers.subList(0, inst.getRegCount()))
                .newLine();
    }

    @Override
    public void visitTODO(BasicBlock bb, int position, Instruction inst) {
        writer.writeIndex(position)
                .writeKeyword(inst.opcode.toString().toLowerCase())
                .writeSpace().write(inst.getFormat())
                .newLine();
    }

    @Override
    public void after(BasicBlock bb) {
        writer.after(null);
    }

}
