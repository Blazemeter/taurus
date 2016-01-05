package taurusjunit;

import junit.framework.TestCase;

import java.io.*;
import java.net.URL;
import java.util.Properties;

public class CustomRunnerTest extends TestCase {

    private static int getLinesCount(File log) throws IOException {
        LineNumberReader reader = new LineNumberReader(new FileReader(log));
        reader.skip(Long.MAX_VALUE);
        reader.close();
        return reader.getLineNumber();
    }

    public void testMain() throws Exception {
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
        CustomRunner.main(args);

        assertEquals(3, getLinesCount(log));
        assertEquals(8, getLinesCount(err));
    }

    public void testIterations() throws Exception {
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
        CustomRunner.main(args);

        assertEquals(7, getLinesCount(log));
        assertEquals(24, getLinesCount(err));
    }

    public void testHold() throws Exception {
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
        props.setProperty(CustomRunner.HOLD, String.valueOf(60));

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        CustomRunner.main(args);

        assertTrue(3 < getLinesCount(log));
        assertTrue(8 < getLinesCount(err));
    }

    public void testHoldIterations() throws Exception {
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
        props.setProperty(CustomRunner.HOLD, String.valueOf(60));
        props.setProperty(CustomRunner.ITERATIONS, String.valueOf(1));

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        CustomRunner.main(args);

        assertEquals(3, getLinesCount(log));
        assertEquals(8, getLinesCount(err));
    }
}