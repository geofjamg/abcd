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

package fr.jamgotchian.abcd.core.region;

import fr.jamgotchian.abcd.core.controlflow.BasicBlock;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RegionName {

    private final BasicBlock block;

    private int index;

    public RegionName(BasicBlock block) {
        this(block, 0);
    }

    private RegionName(BasicBlock block, int index) {
        this.block = block;
        this.index = index;
    }

    public RegionName getParent() {
        return new RegionName(block, index + 1);
    }

    @Override
    public String toString() {
        if (block == null) {
            return "[]";
        } else {
            StringBuilder builder = new StringBuilder(block.toString());
            if (index > 0) {
                builder.append("-").append((char)('a' + index-1));
            }
            return builder.toString();
        }
    }
}
