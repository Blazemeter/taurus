import org.junit.After;
import org.junit.Before;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.concurrent.TimeUnit;
import java.util.Date;
import java.io.File;

public class JUnitTest {
    @Test
    public void aTest() {
        assertEquals("Hello, " + "World!", "Hello, World!");
    }
}
