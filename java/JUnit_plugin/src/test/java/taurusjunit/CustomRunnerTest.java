package taurusjunit;

import junit.framework.TestCase;

import java.io.*;
import java.net.URL;
import java.util.Properties;

public class CustomRunnerTest extends TestCase {

    private static void assertLinesCount(long expected, File log) throws IOException {
        LineNumberReader reader = new LineNumberReader(new FileReader(log));
        reader.skip(Long.MAX_VALUE);
        assertEquals(expected, reader.getLineNumber());
        reader.close();
    }

    public void testMain() throws Exception {
        CustomRunner obj = new CustomRunner();
        File log = File.createTempFile("log", ".jtl");
        log.deleteOnExit();

        File err = File.createTempFile("err", ".jtl");
        err.deleteOnExit();

        URL res = Thread.currentThread().getContextClassLoader().getResource("dummy.jar");
        assert res != null;

        Properties props = new Properties();
        props.setProperty(CustomRunner.KPI_LOG, log.getAbsolutePath());
        props.setProperty(CustomRunner.ERROR_LOG, err.getAbsolutePath());
        props.setProperty(CustomRunner.TARGET_PREFIX + "jar", res.getPath());

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        obj.main(args);

        assertLinesCount(3, log);
        assertLinesCount(8, err);
    }

    public void testIterations() throws Exception {
        CustomRunner obj = new CustomRunner();
        File log = File.createTempFile("log", ".jtl");
        log.deleteOnExit();

        File err = File.createTempFile("err", ".jtl");
        err.deleteOnExit();

        URL res = Thread.currentThread().getContextClassLoader().getResource("dummy.jar");
        assert res != null;

        Properties props = new Properties();
        props.setProperty(CustomRunner.KPI_LOG, log.getAbsolutePath());
        props.setProperty(CustomRunner.ERROR_LOG, err.getAbsolutePath());
        props.setProperty(CustomRunner.TARGET_PREFIX + "jar", res.getPath());
        props.setProperty(CustomRunner.ITERATIONS, String.valueOf(3));

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        obj.main(args);

        assertLinesCount(7, log);
        assertLinesCount(24, err);
    }
}