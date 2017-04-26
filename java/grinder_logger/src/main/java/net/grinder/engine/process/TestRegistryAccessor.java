package net.grinder.engine.process;

import net.grinder.common.GrinderProperties;
import net.grinder.common.Test;
import net.grinder.script.Grinder;
import net.grinder.script.InternalScriptContext;
import net.grinder.script.TestRegistry;
import net.grinder.scriptengine.CompositeInstrumenter;
import net.grinder.statistics.StatisticsAccessor;
import net.grinder.statistics.StatisticsSetFactory;

import java.util.ArrayList;
import java.util.Collection;

public class TestRegistryAccessor {
    private static final Collection<Test> empty = new ArrayList<>();

    public static Collection<Test> getNewTests() {
        TestRegistry reg = Grinder.grinder.getTestRegistry();
        if (reg != null && reg instanceof TestRegistryImplementation) {
            synchronized (reg) {
                TestRegistryImplementation registry = (TestRegistryImplementation) reg;
                Collection<Test> newTests = registry.getNewTests();
                if (newTests != null) {
                    return newTests;
                }
            }
        }
        return empty;
    }

    private static TestRegistry getInstance() {
        StatisticsSetFactory ssf = StatisticsAccessor.getFactory();
        TestRegistryImplementation testRegistryImplementation = new TestRegistryImplementation(null, ssf, null, null);
        testRegistryImplementation.setInstrumenter(new CompositeInstrumenter());
        return testRegistryImplementation;
    }

    public static InternalScriptContext getDummyScriptContext() {
        TestRegistry reg = TestRegistryAccessor.getInstance();
        GrinderProperties props = new GrinderProperties();
        return new ScriptContextImplementation(null, null, null, props, null, null, null, null, reg,
                null, null, null, null);
    }
}
