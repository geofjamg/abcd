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

package fr.jamgotchian.abcd.core;

import fr.jamgotchian.abcd.core.Summary.ErrorInfo;
import fr.jamgotchian.abcd.core.ast.AbstractSyntaxTreeBuilder;
import fr.jamgotchian.abcd.core.ast.Class;
import fr.jamgotchian.abcd.core.ast.CompilationUnit;
import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.stmt.CommentStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.ast.stmt.Statements;
import fr.jamgotchian.abcd.core.ast.expr.LocalVariable;
import fr.jamgotchian.abcd.core.ast.util.Refactorer;
import fr.jamgotchian.abcd.core.ast.util.ForLoopRefactorer;
import fr.jamgotchian.abcd.core.bytecode.MethodFactory;
import fr.jamgotchian.abcd.core.bytecode.DataSource;
import fr.jamgotchian.abcd.core.bytecode.ClassFactory;
import fr.jamgotchian.abcd.core.bytecode.dalvik.DexFileDataSource;
import fr.jamgotchian.abcd.core.bytecode.java.ClassFileDataSource;
import fr.jamgotchian.abcd.core.bytecode.java.JarFileDataSource;
import fr.jamgotchian.abcd.core.common.ABCDWriter;
import fr.jamgotchian.abcd.core.ir.IntermediateRepresentationBuilder;
import fr.jamgotchian.abcd.core.ir.InstructionBuilder;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraphBuilder;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ir.LocalVariableTable;
import fr.jamgotchian.abcd.core.ir.IRInstFactory;
import fr.jamgotchian.abcd.core.ir.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.ir.RPST;
import fr.jamgotchian.abcd.core.ir.Region;
import fr.jamgotchian.abcd.core.ir.RegionAnalysis;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import fr.jamgotchian.abcd.core.util.Exceptions;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Modifier;
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
public class ABCDContext {

    public static final boolean DEBUG = false;

    private static final Logger logger = Logger.getLogger(ABCDContext.class.getName());

    private static List<Refactorer> REFACTORERS = Collections.unmodifiableList(
            Arrays.<Refactorer>asList(new ForLoopRefactorer()));

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
        OPTIONS = new Options();
        OPTIONS.addOptionGroup(file)
                .addOption(outputDir)
                .addOption(classDir)
                .addOption(debugDir);
    }

    public ABCDContext() {
    }

    private void decompile(DataSource dataSrc, ABCDWriter writer) throws IOException {
        Summary summary = new Summary();
        for (ClassFactory classFactory : dataSrc.createClassFactories()) {
            ImportManager importManager = new ImportManager();
            Class _class = decompileClass(classFactory, importManager, writer, summary);

            CompilationUnit compilUnit = new CompilationUnit(_class.getPackage(), importManager);
            compilUnit.getClasses().add(_class);

            writer.writeAST(compilUnit);
        }

        ConsoleUtil.logTitledSeparator(logger, Level.FINE, "Summary", '#');

        logger.log(Level.FINE, "Number of successes : {0}",
                Integer.toString(summary.getNumberOfSuccesses()));
        logger.log(Level.FINE, "Number of failures : {0}",
                Integer.toString(summary.getNumberOfFailures()));
        logger.log(Level.FINE, "Number of class perfectly decompiled : {0}",
                Integer.toString(summary.getNumberOfClassesPerfectlyDecompiled()));
        logger.log(Level.FINE, "Number of class partially decompiled : {0}",
                Integer.toString(summary.getNumberOfClassesPartiallyDecompiled()));
        List<String> errorColumn = new ArrayList<String>(summary.getErrors().size()+1);
        List<String> classColumn = new ArrayList<String>(summary.getErrors().size()+1);
        List<String> methodColumn = new ArrayList<String>(summary.getErrors().size()+1);
        errorColumn.add("Error");
        classColumn.add("Class");
        methodColumn.add("Method");
        for (ErrorInfo error : summary.getErrors()) {
            errorColumn.add(error.getMessage());
            classColumn.add(error.getClassName());
            methodColumn.add(error.getMethodSignature());
        }
        logger.log(Level.FINE, "\n{0}", ConsoleUtil.printTable(errorColumn, classColumn, methodColumn));
    }

    private Class decompileClass(ClassFactory classFactory, ImportManager importManager,
                                 ABCDWriter writer, Summary summary) throws IOException {
        Class _class = classFactory.createClass(importManager);

        ConsoleUtil.logTitledSeparator(logger, Level.FINE, "Decompile class {0}",
                '#', _class.getQualifiedName());

        boolean error = false;

        Collection<MethodFactory> methodFactories = classFactory.createMethodFactories();
        for (MethodFactory methodFactory : methodFactories) {
            Method method = methodFactory.createMethod(importManager);
            _class.addMethod(method);

            // TODO : interfaces and abstract methods
            if (method.getModifiers().contains(Modifier.ABSTRACT)) {
                continue;
            }

            String methodSignature = method.getSignature();

            try {
                logger.log(Level.FINE, "");
                ConsoleUtil.logTitledSeparator(logger, Level.FINE, "Decompile method {0}",
                        '%', methodSignature);
                logger.log(Level.FINE, "");

                logger.log(Level.FINER, "Bytecode :\n{0}", methodFactory.getBytecodeAsText());

                ControlFlowGraphBuilder cfgBuilder
                        = methodFactory.createCFGBuilder(methodSignature);

                TemporaryVariableFactory tmpVarFactory = new TemporaryVariableFactory();

                IRInstFactory instFactory = new IRInstFactory();

                InstructionBuilder instBuilder
                    = methodFactory.createInstBuilder(importManager, tmpVarFactory, instFactory);

                ControlFlowGraph cfg
                        = new IntermediateRepresentationBuilder(cfgBuilder,
                                                                instBuilder,
                                                                importManager,
                                                                tmpVarFactory,
                                                                instFactory).build(writer);

                // analyse local variables types
//                new LocalVariableTypeAnalyser(cfg, method, importManager,
//                                              instFactory).analyse();

                LocalVariableTable table = cfg.getLocalVariableTable();
                for (LocalVariableDeclaration decl : method.getArguments()) {
                    LocalVariable var = decl.getVariable();
                    var.setName(table.getName(var.getID().getIndex(), 0));
                }

                ConsoleUtil.logTitledSeparator(logger, Level.FINE, "Analyse regions of {0}",
                        '=', methodSignature);

                RPST rpst = new RegionAnalysis(cfg).analyse();

                writer.writeRPST(rpst);

                StringBuilder builder = new StringBuilder();
                rpst.print(builder);
                logger.log(Level.FINER, "RPST :\n{0}", builder.toString());

                ConsoleUtil.logTitledSeparator(logger, Level.FINE, "Build AST of {0}",
                        '=', methodSignature);

                Region rootRegion = rpst.getRootRegion();
                new AbstractSyntaxTreeBuilder(cfg, importManager,
                                              rootRegion, method.getBody()).build();

                // refactor AST
//                for (Refactorer refactorer : REFACTORERS) {
//                    refactorer.refactor(method.getBody());
//                }

                summary.incrNumberOfSuccesses();
            } catch (Throwable exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);

                method.getBody().clear();
                StringBuilder msg = new StringBuilder();
                msg.append(Exceptions.printStackTrace(exc))
                   .append("\n")
                   .append(methodFactory.getBytecodeAsText());

                method.getBody().add(new CommentStatement("\n" + msg.toString()));
                method.getBody().add(Statements.createThrowErrorStmt(InternalError.class,
                                                                     "Decompilation failed",
                                                                     importManager));

                error = true;
                summary.incrNumberOfFailures();
                summary.addError(new ErrorInfo(_class.getQualifiedName(),
                                                       methodSignature,
                                                       exc.toString()));
            }
        }

        if (error) {
            summary.incrNumberOfClassesPartiallyDecompiled();
        } else {
            summary.incrNumberOfClassesPerfectlyDecompiled();
        }

        return _class;
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
                    writer = new DebugABCDWriter(DEBUG, outDir, debugDir);
                } else {
                    writer = new DefaultABCDWriter(DEBUG, outDir);
                }

                DataSource dataSrc = null;
                if (line.hasOption("class")) {
                    String className = line.getOptionValue("class");
                    if (line.hasOption("classdir")) {
                        File classDir = new File(line.getOptionValue("classdir"));
                        checkDir(classDir);
                        dataSrc = new ClassFileDataSource(classDir, className);
                    } else {
                        printError("classdir option is mandatory with class option");
                    }
                } else if (line.hasOption("jar")) {
                    File jarFile = new File(line.getOptionValue("jar"));
                    checkFile(jarFile);
                    dataSrc = new JarFileDataSource(new JarFile(jarFile));
                } else { // line.hasOption("dex")
                    File dexFile = new File(line.getOptionValue("dex"));
                    checkFile(dexFile);
                    dataSrc = new DexFileDataSource(dexFile);
                }

                new ABCDContext().decompile(dataSrc, writer);
            }
            catch(ParseException e) {
                printError(e.getMessage());
            }
        } catch (Throwable exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
    }
}
