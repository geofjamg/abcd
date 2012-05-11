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
package fr.jamgotchian.abcd.core.type;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public enum ComputationalType {
    INT(Category.CATEGORY_1),
    FLOAT(Category.CATEGORY_1),
    REFERENCE(Category.CATEGORY_1),
    RETURN_ADDRESS(Category.CATEGORY_1),
    LONG(Category.CATEGORY_2),
    DOUBLE(Category.CATEGORY_2);

    public enum Category {
        CATEGORY_1,
        CATEGORY_2
    }

    private final Category category;

    private ComputationalType(Category category) {
        this.category = category;
    }

    public Category getCategory() {
        return category;
    }
}
