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

import fr.jamgotchian.abcd.core.bytecode.ABCDDataSource;
import fr.jamgotchian.abcd.core.bytecode.dalvik.DexFileDataSource;
import fr.jamgotchian.abcd.core.bytecode.java.ClassFileDataSource;
import fr.jamgotchian.abcd.core.bytecode.java.JarFileDataSource;
import fr.jamgotchian.abcd.core.common.ABCDWriter;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Main {

    private static final Logger LOGGER = Logger.getLogger(Main.class.getName());

    private static final Options OPTIONS;

    static {
        Option classFile = OptionBuilder.withArgName("file")
                            .hasArg()
                            .withDescription("to decompile a class file")
                            .create("class");
        Option jarFile = OptionBuilder.withArgName("file")
                            .hasArg()
                            .withDescription("to decompile a jar file")
                            .create("jar");
        Option dexFile = OptionBuilder.withArgName("file")
                            .hasArg()
                            .withDescription("to decompile a dex file")
                            .create("dex");
        OptionGroup file = new OptionGroup();
        file.addOption(classFile);
        file.addOption(jarFile);
        file.addOption(dexFile);
        file.setRequired(true);
        Option outputDir = OptionBuilder.withArgName("dir")
                            .hasArg()
                            .withDescription("directory where to write java sources")
                            .isRequired(true)
                            .create("d");
        Option classDir = OptionBuilder.withArgName("dir")
                            .hasArg()
                            .withDescription("directory where to find classes")
                            .create("classdir");
        Option debugDir = OptionBuilder.withArgName("dir")
                            .hasArg()
                            .withDescription("directory where to write analysis data")
                            .create("debug");
        Option useLvtDir = OptionBuilder.hasArg(false)
                            .isRequired(false)
                            .withDescription("use local variable table")
                            .create("uselvt");
        OPTIONS = new Options();
        OPTIONS.addOptionGroup(file)
                .addOption(outputDir)
                .addOption(classDir)
                .addOption(debugDir)
                .addOption(useLvtDir);
    }

    private Main() {
    }

    private static void printUsage() {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("abcd", OPTIONS);
        System.exit(-1);
    }

    private static void printError(String msg) {
        System.err.println(msg);
        printUsage();
    }

    private static void checkDir(File dir) {
        if (!dir.exists()) {
            printError(dir + " does not exist");
        }
        if (!dir.isDirectory()) {
            printError(dir + " is not a directory");
        }
    }

    private static void checkFile(File file) {
        if (!file.exists()) {
            printError(file + " does not exist");
        }
        if (!file.isFile()) {
            printError(file + " is not a file");
        }
    }
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        try {
            CommandLineParser parser = new GnuParser();
            try {
                CommandLine line = parser.parse(OPTIONS, args);
                File outDir = new File(line.getOptionValue("d"));
                checkDir(outDir);

                ABCDWriter writer = null;
                if (line.hasOption("debug")) {
                    File debugDir = new File(line.getOptionValue("debug"));
                    checkDir(debugDir);
                    writer = new DebugABCDWriter(outDir, debugDir);
                } else {
                    writer = new DefaultABCDWriter(outDir);
                }

                ABCDDataSource dataSrc = null;
                ClassLoader classLoader = null;
                if (line.hasOption("class")) {
                    String className = line.getOptionValue("class");
                    if (line.hasOption("classdir")) {
                        File classDir = new File(line.getOptionValue("classdir"));
                        checkDir(classDir);
                        dataSrc = new ClassFileDataSource(classDir, className);
                        classLoader = new URLClassLoader(new URL[] {classDir.toURI().toURL()});
                    } else {
                        printError("classdir option is mandatory with class option");
                    }
                } else if (line.hasOption("jar")) {
                    File jarFile = new File(line.getOptionValue("jar"));
                    checkFile(jarFile);
                    dataSrc = new JarFileDataSource(new JarFile(jarFile));
                    classLoader = new URLClassLoader(new URL[] {jarFile.toURI().toURL()});
                } else { // line.hasOption("dex")
                    File dexFile = new File(line.getOptionValue("dex"));
                    checkFile(dexFile);
                    dataSrc = new DexFileDataSource(dexFile);
                    classLoader = new URLClassLoader(new URL[] {dexFile.toURI().toURL()});
                }

                ABCDPreferences prefs = new ABCDPreferencesImpl();
                if (line.hasOption("uselvt")) {
                    prefs.setUseLocalVariableTable(true);
                }

                new ABCDContext().decompile(dataSrc, writer, prefs, classLoader);
            }
            catch(ParseException e) {
                printError(e.getMessage());
            }
        } catch (Throwable exc) {
            LOGGER.log(Level.SEVERE, exc.toString(), exc);
        }
    }
}
