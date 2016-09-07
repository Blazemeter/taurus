package framework;

import org.junit.AfterClass;
import org.junit.BeforeClass;

public class BaseTestClassForBaseJunitTest {
    @BeforeClass
    public static void beforeSuite() {
        System.out.println("beforeSuite");
    }

    @AfterClass
    public static void afterClass() {
        System.out.println("afterSuite");
    }
}
