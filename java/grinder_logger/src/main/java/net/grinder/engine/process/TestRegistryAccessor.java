package net.grinder.engine.process;

import net.grinder.common.GrinderProperties;
import net.grinder.common.Test;
import net.grinder.script.Grinder;
import net.grinder.script.InternalScriptContext;
import net.grinder.script.TestRegistry;
import net.grinder.scriptengine.CompositeInstrumenter;
import net.grinder.statistics.StatisticsAccessor;
import net.grinder.statistics.StatisticsSetFactory;
import net.grinder.statistics.TestStatisticsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

public class TestRegistryAccessor {
    private static final Collection<Test> empty = new ArrayList<>();
    private final static Logger LOGGER = LoggerFactory.getLogger("worker.bzt");

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

    public static void changeTestStatisticsMap() {
        TestRegistry reg = Grinder.grinder.getTestRegistry();
        if (reg != null && reg instanceof TestRegistryImplementation) {
            synchronized (reg) {
                changeInnerMap(((TestRegistryImplementation) reg).getTestStatisticsMap());
            }
        }
    }

    private static void changeInnerMap(TestStatisticsMap testStatisticsMap) {
        try {
            Field data = TestStatisticsMap.class.getDeclaredField("m_data");
            data.setAccessible(true);
            Map map = (Map) data.get(testStatisticsMap);
            data.set(testStatisticsMap, new CustomTreeMap(map));
        } catch (Throwable ex) {
            LOGGER.warn("Cannot change 'm_data' map in TestStatisticsMap", ex);
        }
    }

    public static class CustomTreeMap<K,V> extends TreeMap<K,V> {
        public CustomTreeMap(Map<? extends K, ? extends V> m) {
            super(m);
        }

        @Override
        public V put(K key, V value) {
            printLogInfo(key);
            return super.put(key, value);
        }

        public void printLogInfo(K key) {
            if (key instanceof Test) {
                Test t = (Test) key;
                LOGGER.info("Test name for ID " + t.getNumber() + ": " + t.getDescription());
            }
        }
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
