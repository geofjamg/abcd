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

import java.util.Arrays;
import org.jf.dexlib.Code.Instruction;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CodeAddressManager {

    private final int[] addresses;

    private final int[] positions;

    public CodeAddressManager(Instruction[] instructions) {
        if (instructions.length > 0) {
            addresses = new int[instructions.length];
            int currentAddress = 0;
            addresses[0] = 0;
            for (int pos = 1; pos < instructions.length; pos++) {
                currentAddress += instructions[pos-1].getSize(currentAddress);
                addresses[pos] = currentAddress;
            }
            positions = new int[currentAddress+1];
            Arrays.fill(positions, -1);
            for (int pos = 0; pos < addresses.length; pos++) {
                positions[addresses[pos]] = pos;
            }
        } else {
            addresses = null;
            positions = null;
        }
    }

    public int getAddress(int position) {
        return addresses[position];
    }

    public int getPosition(int address) {
        return positions[address];
    }

    public int getTargetPosition(int position, int targetAddressOffset) {
        int address = addresses[position];
        int targetAddress = address + targetAddressOffset;
        int targetPosition = positions[targetAddress];
        return targetPosition;
    }
}
