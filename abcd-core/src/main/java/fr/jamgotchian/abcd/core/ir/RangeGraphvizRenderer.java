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
package fr.jamgotchian.abcd.core.ir;

import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RangeGraphvizRenderer implements GraphvizRenderer<BasicBlock> {

    @Override
    public Map<String, String> getAttributes(BasicBlock bb) {
        Map<String, String> attrs = new HashMap<>(4);
        attrs.put("shape", "box");
        attrs.put("color", "black");
        if (bb.getType() != null) {
            switch (bb.getType()) {
                case ENTRY:
                case EXIT:
                    attrs.put("shape", "ellipse");
                    attrs.put("color", "lightgrey");
                    attrs.put("style", "filled");
                    break;

                case JUMP_IF:
                    attrs.put("shape", "diamond");
                    attrs.put("color", "cornflowerblue");
                    attrs.put("style", "filled");
                    break;

                case SWITCH:
                    attrs.put("shape", "hexagon");
                    attrs.put("color", "chocolate");
                    attrs.put("style", "filled");
                    break;

                case RETURN:
                    attrs.put("shape", "invhouse");
                    attrs.put("color", "orange");
                    attrs.put("style", "filled");
                    break;

                case GOTO:
                    attrs.put("shape", "invhouse");
                    attrs.put("color", "green");
                    attrs.put("style", "filled");
                    break;

                case THROW:
                    attrs.put("shape", "invhouse");
                    attrs.put("color", "pink");
                    attrs.put("style", "filled");
                    break;

                case BREAK:
                    attrs.put("shape", "invhouse");
                    attrs.put("color", "purple");
                    attrs.put("style", "filled");
                    break;
            }
        }
        attrs.put("label", "\"" + bb + "\"");
        return attrs;
    }
}
