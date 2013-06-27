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
package fr.jamgotchian.abcd.core;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Summary {

    private int numberOfSuccesses = 0;

    private int numberOfFailures = 0;

    private int numberOfClassesPerfectlyDecompiled = 0;

    private int numberOfClassesPartiallyDecompiled = 0;

    public static class ErrorInfo {

        private final String className;

        private final String methodSignature;

        private final String message;

        public ErrorInfo(String className, String methodSignature, String message) {
            this.className = className;
            this.methodSignature = methodSignature;
            this.message = message;
        }

        public String getClassName() {
            return className;
        }

        public String getMethodSignature() {
            return methodSignature;
        }

        public String getMessage() {
            return message;
        }
    }

    private final List<ErrorInfo> errors = new ArrayList<>();

    public int getNumberOfSuccesses() {
        return numberOfSuccesses;
    }

    public int getNumberOfFailures() {
        return numberOfFailures;
    }

    public int getNumberOfClassesPerfectlyDecompiled() {
        return numberOfClassesPerfectlyDecompiled;
    }

    public int getNumberOfClassesPartiallyDecompiled() {
        return numberOfClassesPartiallyDecompiled;
    }

    public void incrNumberOfSuccesses() {
        numberOfSuccesses++;
    }

    public void incrNumberOfFailures() {
        numberOfFailures++;
    }

    public void incrNumberOfClassesPerfectlyDecompiled() {
        numberOfClassesPerfectlyDecompiled++;
    }

    public void incrNumberOfClassesPartiallyDecompiled() {
        numberOfClassesPartiallyDecompiled++;
    }

    public void addError(ErrorInfo error) {
        errors.add(error);
    }

    public List<ErrorInfo> getErrors() {
        return errors;
    }
}
