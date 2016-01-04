package taurusjunit;

import junit.framework.TestCase;
import org.junit.runner.JUnitCore;

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
    public static final String KPI_LOG = "kpi_log";
    public static final String ERROR_LOG = "error_log";
    public static final String TARGET_PREFIX = "target_";
    public static final String ITERATIONS = "iterations";

    static {
        log.setLevel(Level.FINER);
    }

    public void main(String[] args) throws Exception {
        log.info("Starting: " + Arrays.toString(args));
        if (args.length != 1) {
            throw new IllegalArgumentException("Usage requires 1 parameter, containing path to properties file");
        }

        Properties props = new Properties();
        props.load(new FileReader(args[0]));

        ArrayList<Class> classes = getClasses(props);

        if (classes.isEmpty()) {
            throw new RuntimeException("Nothing to test");
        }

        log.info("Running with classes: " + classes.toString());
        JTLReporter jtlReporter = new JTLReporter(props.getProperty(KPI_LOG));
        JTLErrorReporter jtlErrorReporter = new JTLErrorReporter(props.getProperty(ERROR_LOG));
        CustomListener custom_listener = new CustomListener(jtlReporter, jtlErrorReporter);
        JUnitCore runner = new JUnitCore();
        runner.addListener(custom_listener);

        Long iterations = Long.valueOf(props.getProperty(ITERATIONS, "1"));
        for (int iteration = 0; iteration < iterations; iteration++) {
            runner.run(classes.toArray(new Class[classes.size()]));
        }
        jtlReporter.close();
        jtlErrorReporter.close();
    }

    protected ArrayList<Class> getClasses(Properties props) {
        ArrayList<Class> result = new ArrayList<>(0);

        Enumeration<?> it = props.propertyNames();
        while (it.hasMoreElements()) {
            String propName = (String) it.nextElement();
            if (propName.startsWith(TARGET_PREFIX)) {
                result.addAll(getClasses(props.getProperty(propName)));
            }
        }

        return result;
    }

    protected List<Class<?>> getClasses(String jar_path) {
        List<Class<?>> test_classes = new ArrayList<>(); //List of loaded classes
        try {
            processJAR(test_classes, jar_path);
        } catch (IOException | ClassNotFoundException e) {
            log.warning("Failed to add " + jar_path + "\n" + Utils.getStackTrace(e));
        }
        return test_classes;
    }

    protected void processJAR(List<Class<?>> test_classes, String jar_path) throws IOException, ClassNotFoundException {
        log.info("Processing JAR: " + jar_path);
        JarFile jarFile = new JarFile(jar_path);
        Enumeration<JarEntry> jar_entries_enum = jarFile.entries();

        URL[] urls = {new URL("jar:file:" + jar_path + "!/")};
        URLClassLoader cl = URLClassLoader.newInstance(urls);

        while (jar_entries_enum.hasMoreElements()) {
            JarEntry jar_entry = jar_entries_enum.nextElement();
            if (jar_entry.isDirectory() || !jar_entry.getName().endsWith(".class")) {
                continue;
            }

            String className = jar_entry.getName().substring(0, jar_entry.getName().length() - ".class".length());
            className = className.replace('/', '.');

            Class<?> c = cl.loadClass(className);
            log.info("TestCase.class.isAssignableFrom(" + c.getCanonicalName() + ") = " + TestCase.class.isAssignableFrom(c));
            log.info("has_annotations(" + c.getCanonicalName() + ") = " + has_annotations(c));

            if (TestCase.class.isAssignableFrom(c) || has_annotations(c)) {
                test_classes.add(c);
                log.info("class added to tests: " + c.getCanonicalName());
            }
        }
        jarFile.close();
    }

    protected boolean has_annotations(Class<?> c) {
        for (Method method : c.getDeclaredMethods()) {
            if (method.isAnnotationPresent(org.junit.Test.class)) {
                return true;
            }
        }

        return false;
    }

}
