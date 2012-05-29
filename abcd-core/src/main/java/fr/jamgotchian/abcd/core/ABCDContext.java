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
import fr.jamgotchian.abcd.core.ast.ClassKind;
import fr.jamgotchian.abcd.core.ast.CompilationUnit;
import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.stmt.CommentStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statements;
import fr.jamgotchian.abcd.core.ast.util.Refactorer;
import fr.jamgotchian.abcd.core.ast.util.ForLoopRefactorer;
import fr.jamgotchian.abcd.core.bytecode.MethodFactory;
import fr.jamgotchian.abcd.core.bytecode.ABCDDataSource;
import fr.jamgotchian.abcd.core.bytecode.ClassFactory;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.common.ABCDPreferences;
import fr.jamgotchian.abcd.core.common.ABCDWriter;
import fr.jamgotchian.abcd.core.ir.IntermediateRepresentationBuilder;
import fr.jamgotchian.abcd.core.ir.InstructionBuilder;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraphBuilder;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ir.DebugInfoVariableNameProviderFactory;
import fr.jamgotchian.abcd.core.ir.ExceptionTable;
import fr.jamgotchian.abcd.core.ir.IRInstFactory;
import fr.jamgotchian.abcd.core.ir.VariableFactory;
import fr.jamgotchian.abcd.core.ir.RPST;
import fr.jamgotchian.abcd.core.ir.Region;
import fr.jamgotchian.abcd.core.ir.RegionAnalysis;
import fr.jamgotchian.abcd.core.ir.SimpleVariableNameProviderFactory;
import fr.jamgotchian.abcd.core.ir.VariableNameProviderFactory;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import fr.jamgotchian.abcd.core.util.Exceptions;
import fr.jamgotchian.abcd.core.util.TablePrinter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.lang.model.element.Modifier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ABCDContext {

    private static final Logger LOGGER = LoggerFactory.getLogger(ABCDContext.class);

    private static List<Refactorer> REFACTORERS = Collections.unmodifiableList(
            Arrays.<Refactorer>asList(new ForLoopRefactorer()));

    public ABCDContext() {
    }

    public void decompile(ABCDDataSource dataSrc, ABCDWriter writer,
                          ABCDPreferences prefs, ClassLoader classLoader) throws IOException {
        VariableNameProviderFactory nameProviderFactory;
        if (prefs.isUseLocalVariableTable()) {
            nameProviderFactory = new DebugInfoVariableNameProviderFactory();
        } else {
            nameProviderFactory = new SimpleVariableNameProviderFactory();
        }
        Summary summary = new Summary();
        for (ClassFactory classFactory : dataSrc.createClassFactories()) {
            ImportManager importManager = new ImportManager();
            Class _class = decompileClass(classFactory, importManager,
                                          nameProviderFactory, writer, prefs,
                                          classLoader, summary);

            CompilationUnit compilUnit = new CompilationUnit(_class.getPackage(), importManager);
            compilUnit.getClasses().add(_class);

            writer.writeAST(compilUnit);
        }

        LOGGER.debug(ConsoleUtil.formatTitledSeparator("Summary", '#'));

        LOGGER.debug("Number of successes : {}",
                Integer.toString(summary.getNumberOfSuccesses()));
        LOGGER.debug("Number of failures : {}",
                Integer.toString(summary.getNumberOfFailures()));
        LOGGER.debug("Number of class perfectly decompiled : {}",
                Integer.toString(summary.getNumberOfClassesPerfectlyDecompiled()));
        LOGGER.debug("Number of class partially decompiled : {}",
                Integer.toString(summary.getNumberOfClassesPartiallyDecompiled()));
        TablePrinter printer = ConsoleUtil.newTablePrinter("Error", "Class", "Method");
        for (ErrorInfo error : summary.getErrors()) {
            printer.addRow(error.getMessage(), error.getClassName(), error.getMethodSignature());
        }
        LOGGER.debug("\n{}", printer.toString());
    }

    private Class decompileClass(ClassFactory classFactory, ImportManager importManager,
                                 VariableNameProviderFactory nameProviderFactory,
                                 ABCDWriter writer, ABCDPreferences prefs,
                                 ClassLoader classLoader, Summary summary) throws IOException {
        Class _class = classFactory.createClass(importManager);

        ClassName thisClassName = importManager.newClassName(_class.getQualifiedName());
        JavaType thisType = JavaType.newRefType(thisClassName);

        LOGGER.debug(ConsoleUtil.formatTitledSeparator("Decompile class {}", '#'),
                _class.getQualifiedName());

        boolean error = false;

        Collection<MethodFactory> methodFactories = classFactory.createMethodFactories();
        for (MethodFactory methodFactory : methodFactories) {

            VariableFactory varFactory = new VariableFactory();

            Method method = methodFactory.createMethod(importManager, varFactory);
            _class.addMethod(method);

            if (_class.getKind() == ClassKind.INTERFACE
                    || method.getModifiers().contains(Modifier.ABSTRACT)) {
                continue;
            }

            String methodSignature = method.getSignature();

            ControlFlowGraph cfg = null;

            try {
                LOGGER.debug("");
                LOGGER.debug(ConsoleUtil.formatTitledSeparator("Decompile method {}", '%'),
                        methodSignature);
                LOGGER.debug("");

                LOGGER.trace("Bytecode :\n{}", methodFactory.getBytecodeAsText());

                LOGGER.trace("Argument indexes : {}", varFactory.getArgIndexes());

                ControlFlowGraphBuilder cfgBuilder
                        = methodFactory.createCFGBuilder(methodSignature);

                IRInstFactory instFactory = new IRInstFactory();

                InstructionBuilder instBuilder
                    = methodFactory.createInstBuilder(importManager, varFactory, instFactory);

                cfg = new IntermediateRepresentationBuilder(cfgBuilder,
                                                            instBuilder,
                                                            importManager,
                                                            varFactory,
                                                            instFactory,
                                                            nameProviderFactory,
                                                            thisType,
                                                            method.getReturnType(),
                                                            method.getArguments(),
                                                            writer,
                                                            prefs,
                                                            classLoader)
                        .build();

                LOGGER.debug(ConsoleUtil.formatTitledSeparator("Analyse regions of {}", '='),
                        methodSignature);

                RPST rpst = new RegionAnalysis(cfg, writer).analyse();

                StringBuilder builder = new StringBuilder();
                rpst.print(builder);
                LOGGER.trace("RPST :\n{}", builder.toString());

                LOGGER.debug(ConsoleUtil.formatTitledSeparator("Build AST of {}", '='),
                        methodSignature);

                Region rootRegion = rpst.getRootRegion();
                new AbstractSyntaxTreeBuilder(cfg, rootRegion, method.getBody(),
                                              method.getArguments())
                        .build();

                // refactor AST
//                for (Refactorer refactorer : REFACTORERS) {
//                    refactorer.refactor(method.getBody());
//                }

                summary.incrNumberOfSuccesses();
            } catch (Throwable exc) {
                LOGGER.error(exc.toString(), exc);

                method.getBody().clear();
                StringBuilder msg = new StringBuilder();
                msg.append(Exceptions.printStackTrace(exc))
                        .append("\n")
                        .append(methodFactory.getBytecodeAsText())
                        .append("\n");
                if (cfg != null) {
                    ExceptionTable table = cfg.getExceptionTable();
                    if (table != null && table.getEntries().size() > 0) {
                        msg.append("\n");
                        table.print(msg);
                        msg.append("\n");
                    }
                }

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
}
