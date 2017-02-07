package tests;

import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

public class TestNGSuite {
    @Test
    public void good() {
        Assert.assertEquals("Hello" + ", World!", "Hello, World!");
    }

    @Test
    public void bad() {
        Assert.assertEquals(2 + 2 * 2, 8);
    }

    @Test
    public void ugly() {
        throw new SkipException("Skipping the test case");
    }
}
