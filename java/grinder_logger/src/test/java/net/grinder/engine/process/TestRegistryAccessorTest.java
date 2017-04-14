package net.grinder.engine.process;

import net.grinder.script.Grinder;
import net.grinder.script.Test;
import net.grinder.script.TestRegistry;

import static org.junit.Assert.assertEquals;

public class TestRegistryAccessorTest {
    @org.junit.Test
    public void getNewTests_empty() throws Exception {
        TestRegistry reg = TestRegistryAccessor.getInstance();
        Grinder.grinder = new ScriptContextImplementation(null, null, null, null, null, null, null, null, reg,
                null, null, null, null);

        assertEquals(0, TestRegistryAccessor.getNewTests().size());
        new Test(1, "label");
        assertEquals(1, TestRegistryAccessor.getNewTests().size());
        assertEquals(0, TestRegistryAccessor.getNewTests().size());
    }

}