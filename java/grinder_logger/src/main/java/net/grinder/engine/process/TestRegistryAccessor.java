package net.grinder.engine.process;

import net.grinder.common.Test;
import net.grinder.script.Grinder;
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
            TestRegistryImplementation registry = (TestRegistryImplementation) reg;
            Collection<Test> newTests = registry.getNewTests();
            if (newTests != null) {
                return newTests;
            }
        }
        return empty;
    }

    public static TestRegistry getInstance() {
        StatisticsSetFactory ssf = StatisticsAccessor.getFactory();
        TestRegistryImplementation testRegistryImplementation = new TestRegistryImplementation(null, ssf, null, null);
        testRegistryImplementation.setInstrumenter(new CompositeInstrumenter());
        return testRegistryImplementation;
    }
}
