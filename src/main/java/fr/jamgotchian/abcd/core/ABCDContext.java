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
import fr.jamgotchian.abcd.core.bytecode.ClassDataSource;
import fr.jamgotchian.abcd.core.bytecode.JarDataSource;
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

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ABCDContext {

    public static final boolean DEBUG = false;

    private static final Logger logger = Logger.getLogger(ABCDContext.class.getName());

    private static List<Refactorer> REFACTORERS = Collections.unmodifiableList(
            Arrays.<Refactorer>asList(new ForLoopRefactorer()));

    public ABCDContext() {
    }

    private void decompile(DataSource dataSrc, ABCDWriter writer) throws IOException {
        for (ClassFactory classFactory : dataSrc.createClassFactories()) {
            ImportManager importManager = new ImportManager();
            Class _class = decompileClass(classFactory, importManager, writer);

            CompilationUnit compilUnit = new CompilationUnit(_class.getPackage(), importManager);
            compilUnit.getClasses().add(_class);

            writer.writeAST(compilUnit);
        }
    }

    private Class decompileClass(ClassFactory classFactory, ImportManager importManager,
                                 ABCDWriter writer) throws IOException {
        Class _class = classFactory.createClass(importManager);

        ConsoleUtil.logTitledSeparator(logger, Level.FINE, "Decompile class {0}",
                '#', _class.getQualifiedName());

        List<String> errorMsgs = new ArrayList<String>();

        Collection<MethodFactory> methodFactories = classFactory.createMethodFactories();
        for (MethodFactory methodFactory : methodFactories) {
            Method method = methodFactory.createMethod(importManager);
            _class.addMethod(method);

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
                new LocalVariableTypeAnalyser(cfg, method, importManager,
                                              instFactory).analyse();

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

            } catch (Exception exc) {
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

                errorMsgs.add(exc.toString() + " (" + methodSignature + ")");
            }
        }

        ConsoleUtil.logTitledSeparator(logger, Level.FINE, "Summary {0}",
                        '#', _class.getQualifiedName());

        logger.log(Level.FINE, "Succeed : {0}/{1}",
                new Object[]{methodFactories.size() - errorMsgs.size(), methodFactories.size()});
        StringBuilder errorStr = new StringBuilder();
        for (String errorMsg : errorMsgs) {
            errorStr.append("  ").append(errorMsg).append("\n");
        }
        logger.log(Level.FINE, "Failed : {0}/{1}\n{2}",
                new Object[]{errorMsgs.size(), methodFactories.size(),
                    errorStr.toString()});

        return _class;
    }

    private static void printUsage() {
        System.out.println("Usage:");
        System.out.println("    [options] -class <class file>");
        System.out.println("             (to decompile a class)");
        System.out.println("    [options] -jar <jar file>");
        System.out.println("             (to decompile a jar)");
        System.out.println("");
        System.out.println("where [options] include:");
        System.out.println("");
        System.out.println("    -d <dir>          Output directory to write java sources");
        System.out.println("    -classdir <dir>   Directory where to find classes");
        System.out.println("    -analysedir <dir> Directory where to write analysis data");
        System.exit(1);
    }

    private static void printError(String msg) {
        System.err.println(msg);
        printUsage();
    }

    public static void main(String[] args) {
        try {
            String className = null;
            String jarName = null;
            String outDirName = null;
            String classDirName = null;
            String debugDirName = null;
            for (int i = 0; i < args.length; i++) {
                String arg = args[i];
                if ("-class".equals(arg)) {
                    className = args[i+1];
                } else if ("-jar".equals(arg)) {
                    jarName = args[i+1];
                } else if ("-classdir".equals(arg)) {
                    classDirName =  args[i+1];
                } else if ("-debug".equals(arg)) {
                    debugDirName = args[i+1];
                } else if ("-d".equals(arg)) {
                    outDirName = args[i+1];
                }
            }
            if (!(className == null ^ jarName == null)) {
                printError("Should specify -class or -jar option");
            }
            if (outDirName == null) {
                printError("Should specify -d option");
            }
            File outDir = new File(outDirName);
            if (!outDir.isDirectory()) {
                printError(outDirName + " should be a directory");
            }
            if (!outDir.exists()) {
                printError(outDirName + " does not exist");
            }

            ABCDWriter writer = null;
            if (debugDirName != null) {
                File debugDir = new File(debugDirName);
                writer = new DebugABCDWriter(DEBUG, outDir, debugDir);
            } else {
                writer = new DefaultABCDWriter(DEBUG, outDir);
            }

            DataSource dataSrc = null;
            if (className != null) {
                if (classDirName == null) {
                    printError("Should specify -classdir option");
                }
                File classDir = new File(classDirName);
                if (!classDir.isDirectory()) {
                    printError(classDirName + " should be a directory");
                }
                if (!classDir.exists()) {
                    printError(classDirName + " does not exist");
                }
                dataSrc = new ClassDataSource(classDir, className);
            } else { // jarName != null
                File jarFile = new File(jarName);
                if (!jarFile.exists()) {
                    printError(jarFile + " does not exist");
                }
                if (!jarFile.isFile()) {
                    printError(jarFile + " should be a file");
                }
                dataSrc = new JarDataSource(new JarFile(jarFile));
            }

            new ABCDContext().decompile(dataSrc, writer);

        } catch (Throwable exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
    }
}
