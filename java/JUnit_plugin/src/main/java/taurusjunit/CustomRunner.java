package taurusjunit;

import junit.framework.TestCase;
import org.junit.runner.JUnitCore;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;

public class CustomRunner {
    private static final Logger log = Logger.getLogger(CustomRunner.class.getName());

    static {
        log.setLevel(Level.FINER);
    }

    public void main(String[] args) throws Exception {
        log.info("Starting");
        if (args.length < 3) {
            throw new IllegalArgumentException("Usage requires at least 3 params");
        }
        //Open Jar files in args, scan them, load test classes, run test suite
        //Last item in args is a writeSample filename
        //redirect stderr to file

        String[] jar_paths = new String[args.length - 2];
        System.arraycopy(args, 2, jar_paths, 0, args.length - 2);
        List<Class<?>> test_classes = getClasses(jar_paths);
        Class[] classes = test_classes.toArray(new Class[test_classes.size()]);

        if (test_classes.isEmpty()) {
            throw new RuntimeException("Nothing to test");
        } else {
            log.info("Running with classes: " + Arrays.toString(classes));
            CustomListener custom_listener = new CustomListener(new JTLReporter(args[0]), new JTLErrorReporter(args[1]));
            JUnitCore runner = new JUnitCore();
            runner.addListener(custom_listener);
            runner.run(classes);
        }
    }

    public boolean has_annotations(Class<?> c) {
        for (Method method : c.getDeclaredMethods()) {
            if (method.isAnnotationPresent(org.junit.Test.class)) {
                return true;
            }
        }

        return false;
    }

    private List<Class<?>> getClasses(String[] jar_paths) {
        List<Class<?>> test_classes = new ArrayList<>(); //List of loaded classes
        for (String jar_path : jar_paths) {
            try {
                processJAR(test_classes, jar_path);
            } catch (IOException | ClassNotFoundException e) {
                log.warning("Failed to add " + jar_path + "\n" + Utils.getStackTrace(e));
            }
        }
        return test_classes;
    }

    private void processJAR(List<Class<?>> test_classes, String jar_path) throws IOException, ClassNotFoundException {
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

}
