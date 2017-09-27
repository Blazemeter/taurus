package taurustestng;

import junit.framework.TestCase;

import java.io.*;
import java.net.URL;
import java.util.Properties;

public class TestNGRunnerTest extends TestCase {

    private static int getLinesCount(File log) throws IOException {
        LineNumberReader reader = new LineNumberReader(new FileReader(log));
        reader.skip(Long.MAX_VALUE);
        reader.close();
        return reader.getLineNumber();
    }

    public void testMain() throws Exception {
        File report = File.createTempFile("report", ".ldjson");
        report.deleteOnExit();

        URL res = Thread.currentThread().getContextClassLoader().getResource("dummy.jar");
        assert res != null;

        Properties props = new Properties();
        props.setProperty(TestNGRunner.REPORT_FILE, report.getAbsolutePath());
        props.setProperty(TestNGRunner.TARGET_PREFIX + "jar", res.getPath());

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        TestNGRunner.main(args);

        assertEquals(3, getLinesCount(report));
    }

    public void testIterations() throws Exception {
        File report = File.createTempFile("report", ".ldjson");
        report.deleteOnExit();

        URL res = Thread.currentThread().getContextClassLoader().getResource("dummy.jar");
        assert res != null;

        Properties props = new Properties();
        props.setProperty(TestNGRunner.REPORT_FILE, report.getAbsolutePath());
        props.setProperty(TestNGRunner.TARGET_PREFIX + "jar", res.getPath());
        props.setProperty(TestNGRunner.ITERATIONS, String.valueOf(3));

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        TestNGRunner.main(args);

        assertEquals(3 * 3, getLinesCount(report));
    }

    public void testHold() throws Exception {
        File report = File.createTempFile("report", ".ldjson");
        report.deleteOnExit();

        URL res = Thread.currentThread().getContextClassLoader().getResource("dummy.jar");
        assert res != null;

        Properties props = new Properties();
        props.setProperty(TestNGRunner.REPORT_FILE, report.getAbsolutePath());
        props.setProperty(TestNGRunner.TARGET_PREFIX + "jar", res.getPath());
        props.setProperty(TestNGRunner.HOLD, String.valueOf(5));

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        TestNGRunner.main(args);

        assertTrue(3 < getLinesCount(report));
    }

    public void testHoldIterations() throws Exception {
        File report = File.createTempFile("report", ".ldjson");
        report.deleteOnExit();

        URL res = Thread.currentThread().getContextClassLoader().getResource("dummy.jar");
        assert res != null;

        Properties props = new Properties();
        props.setProperty(TestNGRunner.REPORT_FILE, report.getAbsolutePath());
        props.setProperty(TestNGRunner.TARGET_PREFIX + "jar", res.getPath());
        props.setProperty(TestNGRunner.HOLD, String.valueOf(5));
        props.setProperty(TestNGRunner.ITERATIONS, String.valueOf(1));

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        TestNGRunner.main(args);

        assertEquals(3, getLinesCount(report));
    }
}
