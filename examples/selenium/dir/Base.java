package dir;

import org.junit.Test;

public abstract class Base {
    @Test
    public void runTests() {
        testMethod();
    }

    abstract public void testMethod();
}
