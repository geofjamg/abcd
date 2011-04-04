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

package fr.jamgotchian.abcd.core.output;

import java.awt.Color;
import java.io.IOException;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public interface InstnWriter {

    void begin() throws IOException;
    
    void writeProperty(String name, Color color) throws IOException;

    void writeFieldOrMethodInstn(int index, int opcode, String scope, String name) throws IOException;

    void writeIIncInstn(int index, int opcode, int var, int incr) throws IOException;

    void writeInstn(int index, int opcode) throws IOException;

    void writeIntInstn(int index, int opcode, int operand) throws IOException;

    void writerJumpInstn(int index, int opcode, int label) throws IOException;

    void writeLabelInstn(int index, int label) throws IOException;

    void writeLdcInstn(int index, int opcode, Object cst) throws IOException;

    void writeLookupSwitchInstn(int index, int opcode, List<Integer> keys, int defaultLabel, List<Integer> labels) throws IOException;

    void writeMultiANewArrayInstn(int index, int opcode, String type, int dims) throws IOException;

    void writeTableSwitchInstn(int index, int opcode, int min, int max, int defaultLabel, List<Integer> labels) throws IOException;

    void writeTypeInstn(int index, int opcode, String type) throws IOException;

    void writeVarInstn(int index, int opcode, int var) throws IOException;

    void writeLineInstn(int index, int line, int startLabel) throws IOException;

    void writeFrameInstn(int index) throws IOException;

    void end() throws IOException;
}
