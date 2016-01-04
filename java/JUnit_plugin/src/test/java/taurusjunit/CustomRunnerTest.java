package taurusjunit;

import junit.framework.TestCase;

import java.io.File;
import java.net.URL;

public class CustomRunnerTest extends TestCase {

    public void testMain() throws Exception {
        CustomRunner obj = new CustomRunner();
        File log = File.createTempFile("log", ".jtl");
        log.deleteOnExit();

        File err = File.createTempFile("err", ".jtl");
        err.deleteOnExit();
        URL res = Thread.currentThread().getContextClassLoader().getResource("dummy.jar");
        assert res != null;
        String testJAR = res.getPath();
        String[] args = {log.getAbsolutePath(), err.getAbsolutePath(), testJAR};
        obj.main(args);
    }
}