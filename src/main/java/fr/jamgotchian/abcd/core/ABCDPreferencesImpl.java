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

import fr.jamgotchian.abcd.core.common.ABCDPreferences;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class ABCDPreferencesImpl implements ABCDPreferences {

    private boolean useLocalVariableTable = false;

    private boolean analyseLocalVariableType = true;

    @Override
    public boolean isUseLocalVariableTable() {
        return useLocalVariableTable;
    }

    @Override
    public void setUseLocalVariableTable(boolean useLocalVariableTable) {
        this.useLocalVariableTable = useLocalVariableTable;
    }

    @Override
    public boolean isAnalyseLocalVariableType() {
        return analyseLocalVariableType;
    }

    @Override
    public void setAnalyseLocalVariableType(boolean analyseLocalVariableType) {
        this.analyseLocalVariableType = analyseLocalVariableType;
    }
}
