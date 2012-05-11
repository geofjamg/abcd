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
package fr.jamgotchian.abcd.core.util;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SimplestFormatter extends Formatter {

    private final DateFormat dateFomatter = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT);
    
    public SimplestFormatter() {
    }

    public String format(LogRecord record) {
        StringBuilder sb = new StringBuilder();

        Date date = new Date(record.getMillis());
        sb.append(dateFomatter.format(date));
        sb.append(" - ");

        sb.append(record.getLevel().getName());
        sb.append(" - ");

        sb.append(formatMessage(record));
        sb.append("\n");

        if (record.getThrown() != null) {
            StringWriter writer = new StringWriter();
            record.getThrown().printStackTrace(new PrintWriter(writer));
            sb.append(writer.toString());
        }
        
        return sb.toString();
    }
}
