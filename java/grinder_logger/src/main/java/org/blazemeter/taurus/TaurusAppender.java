package org.blazemeter.taurus;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.LoggingEvent;
import ch.qos.logback.core.FileAppender;
import net.grinder.common.Test;
import net.grinder.engine.process.TestRegistryAccessor;

import java.io.IOException;

public class TaurusAppender extends FileAppender {
    @Override
    protected void writeOut(Object event) throws IOException {
        for (Test t : TestRegistryAccessor.getNewTests()) {
            LoggingEvent le = new LoggingEvent();
            if (event instanceof LoggingEvent) {
                LoggingEvent proto= (LoggingEvent) event;
                le.setLoggerName(proto.getLoggerName());
            }
            le.setMessage("Test name for ID " + t.getNumber() + ": " + t.getDescription());
            le.setLevel(Level.INFO);
            super.writeOut(le);
        }
        super.writeOut(event);
    }
}
