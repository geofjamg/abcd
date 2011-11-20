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

import java.io.IOException;
import java.net.URL;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ABCDUtil {

    private static final Logger logger = Logger.getLogger(ABCDUtil.class.getName());

    private static final String UNDEFINED = "UNDEFINED";

    public static final String VERSION;

    public static final String HOME_PAGE;

    static {
        Properties props = new Properties();
        URL url = ClassLoader.getSystemResource("abcd.properties");
        String version = UNDEFINED;
        String homePage = UNDEFINED;
        try {
            props.load(url.openStream());
            version = props.getProperty("abcd.version", UNDEFINED);
            homePage = props.getProperty("abcd.homePage", UNDEFINED);
        } catch(IOException e) {
            logger.log(Level.SEVERE, e.toString(), e);
        }
        VERSION = version;
        HOME_PAGE = homePage;
    }

    private ABCDUtil() {
    }
}
