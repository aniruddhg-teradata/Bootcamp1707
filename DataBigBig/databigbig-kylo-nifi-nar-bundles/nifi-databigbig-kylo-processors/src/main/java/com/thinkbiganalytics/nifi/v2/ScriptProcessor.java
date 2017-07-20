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
import org.apache.commons.lang3.StringUtils;
import org.apache.nifi.annotation.behavior.DynamicProperty;
import org.apache.nifi.annotation.behavior.InputRequirement;
import org.apache.nifi.annotation.behavior.InputRequirement.Requirement;
import org.apache.nifi.annotation.behavior.WritesAttribute;
import org.apache.nifi.annotation.behavior.WritesAttributes;
import org.apache.nifi.annotation.documentation.CapabilityDescription;
import org.apache.nifi.annotation.documentation.Tags;
import org.apache.nifi.annotation.lifecycle.OnScheduled;
import org.apache.nifi.annotation.lifecycle.OnUnscheduled;
import org.apache.nifi.components.PropertyDescriptor;
import org.apache.nifi.components.Validator;
import org.apache.nifi.flowfile.FlowFile;
import org.apache.nifi.processor.AbstractProcessor;
import org.apache.nifi.processor.ProcessContext;
import org.apache.nifi.processor.ProcessSession;
import org.apache.nifi.processor.Relationship;
import org.apache.nifi.processor.exception.ProcessException;
import org.apache.nifi.processor.util.StandardValidators;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

@InputRequirement(Requirement.INPUT_FORBIDDEN)
@Tags({"command", "process", "source", "external", "invoke", "script", "restricted"})
@CapabilityDescription("Runs an operating system command specified by the user and writes to a file that command to a FlowFile. If the command is expected "
        + "to be long-running, the Processor can output the partial data on a specified interval. When this option is used, the output is expected to be in textual "
        + "format, as it typically does not make sense to split binary data on arbitrary time-based intervals.")
@DynamicProperty(name = "An environment variable name", value = "An environment variable value", description = "These environment variables are passed to the process spawned by this Processor")
@WritesAttributes({
        @WritesAttribute(attribute = "command", description = "Executed Script(to File)"),
        @WritesAttribute(attribute = "file.name", description = "Output Filename"),
        @WritesAttribute(attribute = "command.arguments", description = "Arguments of the command")
})
public class ScriptProcessor extends AbstractProcessor {

    final static String ATTRIBUTE_COMMAND = "command";
    final static String ATTRIBUTE_COMMAND_ARGS = "command.arguments";
    final static String ATTRIBUTE_FILE_NAME = "file.name";


    public static final PropertyDescriptor COMMAND = new PropertyDescriptor.Builder()
            .name("Command")
            .description("Specifies the command to be executed; if just the name of an executable is provided, it must be in the user's environment PATH.")
            .required(true)
            .expressionLanguageSupported(false)
            .addValidator(StandardValidators.NON_EMPTY_VALIDATOR)
            .build();

    public static final PropertyDescriptor COMMAND_ARGUMENTS = new PropertyDescriptor.Builder()
            .name("Command Arguments")
            .description("The arguments to supply to the executable delimited by white space. White space can be escaped by enclosing it in double-quotes.")
            .required(false)
            .expressionLanguageSupported(true)
            .addValidator(StandardValidators.NON_EMPTY_VALIDATOR)
            .build();

    public static final PropertyDescriptor FILE_NAME = new PropertyDescriptor.Builder()
            .name("Filename")
            .description("Absolute filename the script is writing to")
            .required(true)
            .expressionLanguageSupported(false)
            .addValidator(StandardValidators.NON_EMPTY_VALIDATOR)
            .build();

    public static final PropertyDescriptor WORKING_DIR = new PropertyDescriptor.Builder()
            .name("Working Directory")
            .description("The directory to use as the current working directory when executing the command")
            .expressionLanguageSupported(false)
            .addValidator(StandardValidators.createDirectoryExistsValidator(false, true))
            .required(false)
            .build();

    public static final PropertyDescriptor REDIRECT_ERROR_STREAM = new PropertyDescriptor.Builder()
            .name("Redirect Error Stream")
            .description("If true will redirect any error stream output of the process to the output stream. "
                    + "This is particularly helpful for processes which write extensively to the error stream or for troubleshooting.")
            .required(false)
            .allowableValues("true", "false")
            .defaultValue("false")
            .expressionLanguageSupported(false)
            .addValidator(StandardValidators.BOOLEAN_VALIDATOR)
            .build();

    private static final Validator characterValidator = new StandardValidators.StringLengthValidator(1, 1);

    static final PropertyDescriptor ARG_DELIMITER = new PropertyDescriptor.Builder()
            .name("Argument Delimiter")
            .description("Delimiter to use to separate arguments for a command [default: space]. Must be a single character.")
            .addValidator(Validator.VALID)
            .addValidator(characterValidator)
            .required(true)
            .defaultValue(" ")
            .build();


    public static final Relationship REL_SUCCESS = new Relationship.Builder()
            .name("success")
            .description("All created FlowFiles are routed to this relationship")
            .build();

    private volatile Process externalProcess;

    private volatile ExecutorService executor;
    private Future<?> longRunningProcess;
    private AtomicBoolean failure = new AtomicBoolean(false);

    @Override
    public Set<Relationship> getRelationships() {
        return Collections.singleton(REL_SUCCESS);
    }

    @Override
    protected List<PropertyDescriptor> getSupportedPropertyDescriptors() {
        final List<PropertyDescriptor> properties = new ArrayList<>();
        properties.add(COMMAND);
        properties.add(COMMAND_ARGUMENTS);
        properties.add(FILE_NAME);
        properties.add(REDIRECT_ERROR_STREAM);
        properties.add(ARG_DELIMITER);
        return properties;
    }

    @Override
    protected PropertyDescriptor getSupportedDynamicPropertyDescriptor(final String propertyDescriptorName) {
        return new PropertyDescriptor.Builder()
                .name(propertyDescriptorName)
                .description("Sets the environment variable '" + propertyDescriptorName + "' for the process' environment")
                .dynamic(true)
                .addValidator(StandardValidators.NON_EMPTY_VALIDATOR)
                .build();
    }


    @OnScheduled
    public void setupExecutor(final ProcessContext context) {
        executor = Executors.newFixedThreadPool(context.getMaxConcurrentTasks() * 2, new ThreadFactory() {
            private final ThreadFactory defaultFactory = Executors.defaultThreadFactory();

            @Override
            public Thread newThread(final Runnable r) {
                final Thread t = defaultFactory.newThread(r);
                t.setName("ExecuteProcess " + getIdentifier() + " Task");
                return t;
            }
        });
    }

    @OnUnscheduled
    public void shutdownExecutor() {
        try {
            executor.shutdown();
        } finally {
            if (this.externalProcess.isAlive()) {
                this.getLogger().info("Process hasn't terminated, forcing the interrupt");
                this.externalProcess.destroyForcibly();
            }
        }
    }

    @Override
    public void onTrigger(final ProcessContext context, final ProcessSession session) throws ProcessException {

        final String command = context.getProperty(COMMAND).getValue();
        final String arguments = context.getProperty(COMMAND_ARGUMENTS).isSet()
                ? context.getProperty(COMMAND_ARGUMENTS).evaluateAttributeExpressions().getValue()
                : null;
        final String filename = context.getProperty(FILE_NAME).getValue();

        final List<String> commandStrings = createCommandStrings(context, command, arguments);
        final String commandString = StringUtils.join(commandStrings, " ");

        if (longRunningProcess == null || longRunningProcess.isDone()) {
            try {
                longRunningProcess = launchProcess(context, commandStrings);
            } catch (final IOException ioe) {
                getLogger().error("Failed to create process due to {}", new Object[]{ioe});
                context.yield();
                return;
            }
        } else {
            getLogger().info("Read from long running process");
        }

        if (!isScheduled()) {
            getLogger().info("User stopped processor; will terminate process immediately");
            longRunningProcess.cancel(true);
            return;
        }

        // Create a FlowFile that we can write to and set the OutputStream for the FlowFile
        // as the delegate for the ProxyOuptutStream, then wait until the process finishes
        // or until the specified amount of time
        FlowFile flowFile = session.create();

        try {
            longRunningProcess.get();
        } catch (final InterruptedException ie) {
        } catch (final ExecutionException ee) {
            getLogger().error("Process execution failed due to {}", new Object[]{ee.getCause()});
        }
        // stream
        File toRead = new File(filename);
        flowFile = session.importFrom(toRead.toPath(), false, flowFile);

        if (flowFile.getSize() == 0L) {
            // If no data was written to the file, remove it
            session.remove(flowFile);
        } else if (failure.get()) {
            // If there was a failure processing the output of the Process, remove the FlowFile
            session.remove(flowFile);
            getLogger().error("Failed to read data from Process, so will not generate FlowFile");
        } else {
            // add command and arguments as attribute
            flowFile = session.putAttribute(flowFile, ATTRIBUTE_FILE_NAME, filename);
            flowFile = session.putAttribute(flowFile, ATTRIBUTE_COMMAND, command);
            if (arguments != null) {
                flowFile = session.putAttribute(flowFile, ATTRIBUTE_COMMAND_ARGS, arguments);
            }

            // All was good. Generate event and transfer FlowFile.
            session.getProvenanceReporter().create(flowFile, "Created from command: " + commandString);
            getLogger().info("Created {} and routed to success", new Object[]{flowFile});
            session.transfer(flowFile, REL_SUCCESS);
        }

        // Commit the session so that the FlowFile is transferred to the next processor
        session.commit();
    }

    protected List<String> createCommandStrings(final ProcessContext context, final String command, final String arguments) {
        final List<String> args = ArgumentUtils.splitArgs(arguments, context.getProperty(ARG_DELIMITER).getValue().charAt(0));
        final List<String> commandStrings = new ArrayList<>(args.size() + 1);
        commandStrings.add(command);
        commandStrings.addAll(args);
        return commandStrings;
    }

    protected Future<?> launchProcess(final ProcessContext context, final List<String> commandStrings) throws IOException {

        final Boolean redirectErrorStream = context.getProperty(REDIRECT_ERROR_STREAM).asBoolean();

        final ProcessBuilder builder = new ProcessBuilder(commandStrings);
        final String workingDirName = context.getProperty(WORKING_DIR).getValue();
        if (workingDirName != null) {
            builder.directory(new File(workingDirName));
        }

        final Map<String, String> environment = new HashMap<>();
        for (final Map.Entry<PropertyDescriptor, String> entry : context.getProperties().entrySet()) {
            if (entry.getKey().isDynamic()) {
                environment.put(entry.getKey().getName(), entry.getValue());
            }
        }

        if (!environment.isEmpty()) {
            builder.environment().putAll(environment);
        }

        getLogger().info("Start creating new Process > {} ", new Object[]{commandStrings});
        this.externalProcess = builder.redirectErrorStream(redirectErrorStream).start();

        // Submit task to read error stream from process
        if (!redirectErrorStream) {
            executor.submit(new Runnable() {
                @Override
                public void run() {
                    try (final BufferedReader reader = new BufferedReader(new InputStreamReader(externalProcess.getErrorStream()))) {
                        reader.lines().filter(line -> line != null && line.length() > 0).forEach(getLogger()::warn);
                    } catch (final IOException ioe) {
                    }
                }
            });
        }

        // Submit task to read output of Process and write to FlowFile.
        failure = new AtomicBoolean(false);

        return executor.submit(new Callable<Object>() {
            @Override
            public Object call() throws IOException {
                try {
                    // Since we are going to exit anyway, one sec gives it an extra chance to exit gracefully.
                    // In the future consider exposing it via configuration.
                    boolean terminated = externalProcess.waitFor(1000, TimeUnit.MILLISECONDS);
                    int exitCode = terminated ? externalProcess.exitValue() : -9999;
                    getLogger().info("Process finished with exit code {} ", new Object[]{exitCode});
                } catch (InterruptedException e1) {
                    Thread.currentThread().interrupt();
                }
                return null;
            }
        });
    }
}