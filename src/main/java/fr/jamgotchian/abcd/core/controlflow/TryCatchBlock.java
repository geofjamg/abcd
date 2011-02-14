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

package fr.jamgotchian.abcd.core.controlflow;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TryCatchBlock {
  
    private int tryStart;
        
    private int tryEnd;

    private final int catchStart;

    private final String exceptionClassName;

    public TryCatchBlock(int tryStart, int tryEnd, int catchStart, String exceptionClassName) {
        if (tryEnd <= tryStart) {
            throw new IllegalArgumentException("tryEnd <= tryStart");
        }

        if (catchStart < tryEnd) {
            throw new IllegalArgumentException("catchStart < tryEnd");
        }

        this.tryStart = tryStart;
        this.tryEnd = tryEnd;
        this.catchStart = catchStart;
        this.exceptionClassName = exceptionClassName;
    }

    public TryCatchBlock(TryCatchBlock other) {
        this.tryStart = other.tryStart;
        this.tryEnd = other.tryEnd;
        this.catchStart = other.catchStart;
        this.exceptionClassName = other.exceptionClassName;
    }

    public int getTryStart() {
        return tryStart;
    }

    public int getTryEnd() {
        return tryEnd;
    }

    public int getCatchStart() {
        return catchStart;
    }

    public String getExceptionClassName() {
        return exceptionClassName;
    }

    @Override
    protected TryCatchBlock clone() {
        return new TryCatchBlock(this);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + tryStart + ", " + tryEnd + ", " + 
                catchStart + ", " + exceptionClassName + ")";
    }

}
