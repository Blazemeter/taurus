package taurustestng;

import org.testng.CommandLineArgs;
import org.testng.TestNG;
import org.testng.collections.Lists;

import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;

public class CustomRunner {
    private static final Logger log = Logger.getLogger(CustomRunner.class.getName());
    public static final String REPORT_FILE = "report_file";
    public static final String TARGET_PREFIX = "target_";
    public static final String ITERATIONS = "iterations";
    public static final String HOLD = "hold_for";

    static {
        log.setLevel(Level.FINER);
    }

    public static void main(String[] args) throws Exception {
        log.info("Starting: " + Arrays.toString(args));
        if (args.length != 1) {
            throw new IllegalArgumentException("Usage requires 1 parameter, containing path to properties file");
        }

        Properties props = new Properties();
        props.load(new FileReader(args[0]));

        TaurusReporter reporter = new TaurusReporter(props.getProperty(REPORT_FILE));
        CustomListener customListener = new CustomListener();
        TestNG testng = new TestNG();
        testng.addListener(customListener);
        List<String> suites = Lists.newArrayList();
        suites.add("testng.xml");
        testng.setTestSuites(suites);

        long iterations = Long.valueOf(props.getProperty(ITERATIONS, "0"));
        float hold = Float.valueOf(props.getProperty(HOLD, "0"));
        if (iterations == 0) {
            if (hold > 0) {
                iterations = Long.MAX_VALUE;
            } else {
                iterations = 1;
            }
        }

        long startTime = System.currentTimeMillis();
        for (int iteration = 0; iteration < iterations; iteration++) {
            testng.run();
            log.info("Elapsed: " + (System.currentTimeMillis() - startTime) + ", limit: " + (hold * 1000));
            if (hold > 0 && System.currentTimeMillis() - startTime > hold * 1000) {
                log.info("Duration limit reached, stopping");
                break;
            }
        }

        reporter.close();
    }
}
