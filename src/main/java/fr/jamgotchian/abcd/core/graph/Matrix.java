/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

package fr.jamgotchian.abcd.core.graph;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Matrix<T> {

    private final List<List<T>> values;

    public Matrix(int rowCount, int columnCount, T intialValue) {
        values = new ArrayList<List<T>>(rowCount);
        for (int r = 0; r < rowCount; r++) {
            List<T> row = new ArrayList<T>(columnCount);
            for (int c = 0; c < columnCount; c++) {
                row.add(intialValue);
            }
            values.add(row);
        }
    }
    
    public int getRowCount() {
        return values.size();
    }

    public int getColumnCount() {
        return values.isEmpty() ? 0 : values.get(0).size();
    }
    
    public T getValue(int row, int column) {
        return values.get(row).get(column);
    }
    
    public void setValue(int row, int column, T value) {
        values.get(row).set(column, value);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Matrix)) {
            return false;
        }
        
        Matrix<T> other = (Matrix<T>) obj;
        if (getRowCount() != other.getRowCount()) {
            return false;
        }
        if (getColumnCount() != other.getColumnCount()) {
            return false;
        }
        
        for (int r = 0; r < getRowCount(); r++) {
            for (int c = 0; c < getColumnCount(); c++) {
                if (!getValue(r, c).equals(other.getValue(r, c))) {
                    return false;
                }
            }
        }
        
        return true;
    }

    @Override
    public int hashCode() {
        return getRowCount() + getColumnCount(); // very bad hashcode ...
    }
    
    public void print(PrintStream out) {
        for (int r = 0; r < getRowCount(); r++) {
            for (int c = 0; c < getColumnCount(); c++) {
                out.print(getValue(r, c) + " ");
            }
            out.println();
        }
    }
}
