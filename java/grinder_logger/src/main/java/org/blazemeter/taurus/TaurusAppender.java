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
        if (event instanceof LoggingEvent) {
            LoggingEvent proto = (LoggingEvent) event;
            for (Test t : TestRegistryAccessor.getNewTests()) {
                LoggingEvent le = new LoggingEvent();
                le.setLoggerName(proto.getLoggerName());
                le.setMessage("Test name for ID " + t.getNumber() + ": " + t.getDescription());
                le.setLevel(Level.INFO);
                super.writeOut(le);
            }

            LoggingEvent wrapped = new LoggingEvent();
            if (proto.getLoggerName().equals("data")) {
                wrapped.setLoggerName(proto.getLoggerName() + "." + proto.getLoggerContextVO().getPropertyMap().get("WORKER_NAME"));
            } else {
                wrapped.setLoggerName(proto.getLoggerName());
            }
            wrapped.setMessage(proto.getMessage());
            wrapped.setLevel(proto.getLevel());
            wrapped.setArgumentArray(proto.getArgumentArray());
            super.writeOut(wrapped);

        } else {
            super.writeOut(event);
        }
    }
}
