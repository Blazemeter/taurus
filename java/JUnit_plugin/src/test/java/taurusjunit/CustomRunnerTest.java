package taurusjunit;

import junit.framework.TestCase;

public class CustomRunnerTest extends TestCase {

    public void testMain() throws Exception {
        CustomRunner obj = new CustomRunner();
        String[] args = {"/tmp/1.log", "/tmp/problem-junit/compiled.jar"};
        obj.main(args);
    }
}