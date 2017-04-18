package net.grinder.engine.process;

import net.grinder.script.Grinder;
import net.grinder.script.Test;

import static org.junit.Assert.assertEquals;

public class TestRegistryAccessorTest {
    @org.junit.Test
    public void getNewTests_empty() throws Exception {
        Grinder.grinder = TestRegistryAccessor.getDummyScriptContext();
        assertEquals(0, TestRegistryAccessor.getNewTests().size());
        new Test(1, "label");
        assertEquals(1, TestRegistryAccessor.getNewTests().size());
        assertEquals(0, TestRegistryAccessor.getNewTests().size());
    }

}