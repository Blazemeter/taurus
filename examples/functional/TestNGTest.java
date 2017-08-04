package tests;

import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Parameters;
import org.testng.annotations.Test;

public class TestNGTest {
    @Test
    public void testngTest() {
        Assert.assertEquals(2 + 2, 4);
    }

}
