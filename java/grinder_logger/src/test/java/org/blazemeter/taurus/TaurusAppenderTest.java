package org.blazemeter.taurus;

import ch.qos.logback.classic.spi.LoggingEvent;
import net.grinder.engine.process.TestRegistryAccessor;
import net.grinder.script.Grinder;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TaurusAppenderTest {
    @Test
    public void writeOut() throws Exception {
        Grinder.grinder = TestRegistryAccessor.getDummyScriptContext();

        new net.grinder.script.Test(1, "label");

        TaurusAppender appender = new TaurusAppender();
        appender.writeOut(new LoggingEvent());
    }

}