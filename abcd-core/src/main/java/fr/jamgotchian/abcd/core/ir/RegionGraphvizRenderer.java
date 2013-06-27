/*
 * Copyright (C) 2012 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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
public class RegionGraphvizRenderer implements GraphvizRenderer<Region> {

    @Override
    public Map<String, String> getAttributes(Region region) {
        Map<String, String> attrs = new HashMap<>(4);
        attrs.put("shape", "box");
        attrs.put("color", "black");
        attrs.put("fontsize", "11");
        attrs.put("label", "\"" + region.getParentType() + "\\n" + region.toString() + "\"");
        return attrs;
    }
}
