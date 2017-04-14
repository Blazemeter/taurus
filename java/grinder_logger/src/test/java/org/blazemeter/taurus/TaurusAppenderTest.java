package org.blazemeter.taurus;

import ch.qos.logback.classic.spi.LoggingEvent;
import net.grinder.engine.process.TestRegistryAccessor;
import net.grinder.script.Grinder;
import net.grinder.util.logback.BufferedEchoMessageEncoder;
import org.junit.Test;

import java.io.ByteArrayOutputStream;

import static org.junit.Assert.assertEquals;

public class TaurusAppenderTest {
    @Test
    public void writeOut() throws Exception {
        Grinder.grinder = TestRegistryAccessor.getDummyScriptContext();

        new net.grinder.script.Test(1, "label");

        TaurusAppender appender = new TaurusAppender();
        BufferedEchoMessageEncoder encoder = new BufferedEchoMessageEncoder();
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        encoder.init(os);
        appender.setEncoder(encoder);
        LoggingEvent event = new LoggingEvent();
        event.setMessage("Tada!");
        appender.setOutputStream(os);
        appender.start();
        appender.writeOut(event);
        appender.stop();

        assertEquals("Test name for 1: label\nTada!\n", os.toString());
    }

}