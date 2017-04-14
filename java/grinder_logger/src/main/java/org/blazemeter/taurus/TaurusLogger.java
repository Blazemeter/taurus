package org.blazemeter.taurus;

import ch.qos.logback.core.FileAppender;
import net.grinder.script.Grinder;
import net.grinder.script.TestRegistry;

import java.io.IOException;

public class TaurusLogger extends FileAppender {
    @Override
    protected void writeOut(Object event) throws IOException {

        super.writeOut(event);
    }
}
