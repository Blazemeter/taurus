package taurusjunit;

import junit.framework.TestCase;

import java.io.File;
import java.io.FileWriter;
import java.net.URL;
import java.util.Properties;

public class CustomRunnerTest extends TestCase {

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
        props.setProperty(CustomRunner.TARGET_PREFIX+"jar", res.getPath());

        File propsFile = File.createTempFile("runner", ".properties");
        propsFile.deleteOnExit();
        props.store(new FileWriter(propsFile), "test");

        String[] args = {propsFile.getAbsolutePath()};
        obj.main(args);
    }
}