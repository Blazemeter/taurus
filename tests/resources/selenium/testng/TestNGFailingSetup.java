package tests;

import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.SkipException;
import org.testng.annotations.Test;

public class TestNGFailingSetup {
    @BeforeMethod
    public void setUp() {
        float x = 0 / 0;
    }

    @Test
    public void good() {
        Assert.assertEquals("Hello" + ", World!", "Hello, World!");
    }
}
