/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.thinkbiganalytics.nifi.v2;

import com.thinkbiganalytics.nifi.v2.util.ArgumentUtils;
import org.apache.nifi.processor.ProcessContext;
import org.apache.nifi.util.LogMessage;
import org.apache.nifi.util.MockFlowFile;
import org.apache.nifi.util.TestRunner;
import org.apache.nifi.util.TestRunners;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.io.File;
import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.ExecutorService;

import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.*;

public class ScriptProcessorTest {

    @Test
    public void testSplitArgs() {
        final List<String> nullArgs = ArgumentUtils.splitArgs(null, ' ');
        assertNotNull(nullArgs);
        assertTrue(nullArgs.isEmpty());

        final List<String> zeroArgs = ArgumentUtils.splitArgs("  ", ' ');
        assertNotNull(zeroArgs);
        assertEquals(3, zeroArgs.size());
        String[] expectedArray = {"","",""};
        assertArrayEquals(expectedArray, zeroArgs.toArray(new String[0]));

        final List<String> singleArg = ArgumentUtils.splitArgs("    hello   ", ';');
        assertEquals(1, singleArg.size());
        assertEquals("    hello   ", singleArg.get(0));

        final List<String> twoArg = ArgumentUtils.splitArgs("   hello ;   good-bye   ", ';');
        assertEquals(2, twoArg.size());
        assertEquals("   hello ", twoArg.get(0));
        assertEquals("   good-bye   ", twoArg.get(1));

        final List<String> oneUnnecessarilyQuotedArg = ArgumentUtils.splitArgs("  \"hello\" ", ';');
        assertEquals(1, oneUnnecessarilyQuotedArg.size());
        assertEquals("  hello ", oneUnnecessarilyQuotedArg.get(0));

        final List<String> twoQuotedArg = ArgumentUtils.splitArgs("\"   hello\" \"good   bye\"", ' ');
        assertEquals(2, twoQuotedArg.size());
        assertEquals("   hello", twoQuotedArg.get(0));
        assertEquals("good   bye", twoQuotedArg.get(1));

        final List<String> twoArgOneQuotedPerDelimiterArg = ArgumentUtils.splitArgs("one;two;three\";\"and\";\"half\"", ';');
        assertEquals(3, twoArgOneQuotedPerDelimiterArg.size());
        assertEquals("one", twoArgOneQuotedPerDelimiterArg.get(0));
        assertEquals("two", twoArgOneQuotedPerDelimiterArg.get(1));
        assertEquals("three;and;half", twoArgOneQuotedPerDelimiterArg.get(2));

        final List<String> twoArgOneWholeQuotedArgOneEmptyArg = ArgumentUtils.splitArgs("one;two;\"three;and;half\";", ';');
        assertEquals(4, twoArgOneWholeQuotedArgOneEmptyArg.size());
        assertEquals("one", twoArgOneWholeQuotedArgOneEmptyArg.get(0));
        assertEquals("two", twoArgOneWholeQuotedArgOneEmptyArg.get(1));
        assertEquals("three;and;half", twoArgOneWholeQuotedArgOneEmptyArg.get(2));
        assertEquals("", twoArgOneWholeQuotedArgOneEmptyArg.get(3));
    }


    @Test
    public void validateProcessInterruptOnStop() throws Exception {
        final TestRunner runner = TestRunners.newTestRunner(ScriptProcessor.class);
        runner.setProperty(ScriptProcessor.COMMAND, "ping");
        runner.setProperty(ScriptProcessor.COMMAND_ARGUMENTS, "nifi.apache.org");
        runner.setProperty(ScriptProcessor.FILE_NAME, "Novo_dicionário_da_língua_portuguesa_by_Cândido_de_Figueiredo.txt");

        runner.run();
        Thread.sleep(500);
        ScriptProcessor processor = (ScriptProcessor) runner.getProcessor();
        try {
            Field executorF = ScriptProcessor.class.getDeclaredField("executor");
            executorF.setAccessible(true);
            ExecutorService executor = (ExecutorService) executorF.get(processor);
            assertTrue(executor.isShutdown());
            assertTrue(executor.isTerminated());

            Field processF = ScriptProcessor.class.getDeclaredField("externalProcess");
            processF.setAccessible(true);
            Process process = (Process) processF.get(processor);
            assertFalse(process.isAlive());
        } catch (Exception e) {
            fail();
        }

    }

    // @Test
    public void testBigBinaryInputData() {
        System.setProperty("org.slf4j.simpleLogger.log.org.apache.nifi", "TRACE");
        System.setProperty("org.slf4j.simpleLogger.log.org.apache.nifi.processors.standard", "DEBUG");

        String workingDirName = "/var/test";
        String testFile = "eclipse-java-luna-SR2-win32.zip";

        final TestRunner runner = TestRunners.newTestRunner(ScriptProcessor.class);
        runner.setProperty(ScriptProcessor.COMMAND, "cmd");
        runner.setProperty(ScriptProcessor.COMMAND_ARGUMENTS, " /c type " + testFile);
        runner.setProperty(ScriptProcessor.WORKING_DIR, workingDirName);

        File inFile = new File(workingDirName, testFile);
        System.out.println(inFile.getAbsolutePath());

        runner.run();

        final List<MockFlowFile> flowFiles = runner.getFlowFilesForRelationship(ScriptProcessor.REL_SUCCESS);
        long totalFlowFilesSize = 0;
        for (final MockFlowFile flowFile : flowFiles) {
            System.out.println(flowFile);
            totalFlowFilesSize += flowFile.getSize();
            // System.out.println(new String(flowFile.toByteArray()));
        }

        assertEquals(inFile.length(), totalFlowFilesSize);
    }

    @Test
    public void testBigInputSplit() {
        System.setProperty("org.slf4j.simpleLogger.log.org.apache.nifi", "TRACE");
        System.setProperty("org.slf4j.simpleLogger.log.org.apache.nifi.processors.standard", "DEBUG");

        String workingDirName = "/var/test";
        String testFile = "Novo_dicionário_da_língua_portuguesa_by_Cândido_de_Figueiredo.txt";
        // String testFile = "eclipse-java-luna-SR2-win32.zip";

        final TestRunner runner = TestRunners.newTestRunner(ScriptProcessor.class);
        runner.setProperty(ScriptProcessor.COMMAND, "cmd");
        runner.setProperty(ScriptProcessor.COMMAND_ARGUMENTS, " /c type " + testFile);
        runner.setProperty(ScriptProcessor.WORKING_DIR, workingDirName);
        runner.setProperty(ScriptProcessor.FILE_NAME, "Novo_dicionário_da_língua_portuguesa_by_Cândido_de_Figueiredo.txt");

        File inFile = new File(workingDirName, testFile);
        System.out.println(inFile.getAbsolutePath());

        // runner.run(1,false,true);

        ProcessContext processContext = runner.getProcessContext();

        ScriptProcessor processor = (ScriptProcessor) runner.getProcessor();
        processor.updateScheduledTrue();
        processor.setupExecutor(processContext);

        processor.onTrigger(processContext, runner.getProcessSessionFactory());
        processor.onTrigger(processContext, runner.getProcessSessionFactory());
        processor.onTrigger(processContext, runner.getProcessSessionFactory());
        processor.onTrigger(processContext, runner.getProcessSessionFactory());
        processor.onTrigger(processContext, runner.getProcessSessionFactory());
        processor.onTrigger(processContext, runner.getProcessSessionFactory());
        processor.onTrigger(processContext, runner.getProcessSessionFactory());
        processor.onTrigger(processContext, runner.getProcessSessionFactory());
        processor.onTrigger(processContext, runner.getProcessSessionFactory());

        // runner.run(5,true,false);

        final List<MockFlowFile> flowFiles = runner.getFlowFilesForRelationship(ScriptProcessor.REL_SUCCESS);
        long totalFlowFilesSize = 0;
        for (final MockFlowFile flowFile : flowFiles) {
            System.out.println(flowFile);
            totalFlowFilesSize += flowFile.getSize();
            // System.out.println(new String(flowFile.toByteArray()));
        }

        // assertEquals(inFile.length(), totalFlowFilesSize);
    }

    @Test
    public void testRedirectErrorStream() {
        final TestRunner runner = TestRunners.newTestRunner(ScriptProcessor.class);
        runner.setProperty(ScriptProcessor.COMMAND, "cd");
        runner.setProperty(ScriptProcessor.COMMAND_ARGUMENTS, "does-not-exist");
        runner.setProperty(ScriptProcessor.REDIRECT_ERROR_STREAM, "true");
        runner.setProperty(ScriptProcessor.FILE_NAME, "Novo_dicionário_da_língua_portuguesa_by_Cândido_de_Figueiredo.txt");

        ProcessContext processContext = runner.getProcessContext();

        ScriptProcessor processor = (ScriptProcessor) runner.getProcessor();
        processor.updateScheduledTrue();
        processor.setupExecutor(processContext);

        processor.onTrigger(processContext, runner.getProcessSessionFactory());

        if (isCommandFailed(runner)) return;

        final List<LogMessage> warnMessages = runner.getLogger().getWarnMessages();
        assertEquals("If redirect error stream is true " +
                "the output should be sent as a content of flow-file.", 0, warnMessages.size());
        final List<MockFlowFile> succeeded = runner.getFlowFilesForRelationship(ScriptProcessor.REL_SUCCESS);
        assertEquals(1, succeeded.size());
    }

    /**
     * On some environment, the test command immediately fail with an IOException
     * because of the native UnixProcess.init method implementation difference.
     *
     * @return true, if the command fails
     */
    private boolean isCommandFailed(final TestRunner runner) {
        final List<LogMessage> errorMessages = runner.getLogger().getErrorMessages();
        return (errorMessages.size() > 0
                && errorMessages.stream()
                .anyMatch(m -> m.getMsg().contains("Failed to create process due to")));
    }

}
