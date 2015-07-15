package taurus_junit_listener;

import org.junit.runner.JUnitCore;
import junit.framework.TestCase;
import taurus_junit_listener.CustomListener;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

public class CustomRunner {
	
	
	public static boolean has_annotations(Class<?> c){
		for (Method method : c.getDeclaredMethods()){
			if (method.isAnnotationPresent(org.junit.Test.class)) {
				return true;
			}
		}
		
		return false;
	};
	public static void main(String[] args) {
	//Open Jar files in args, scan them, load test classes, run test suite
	//Last item in args is a report filename
	//redirect stderr to file
	try {
		PrintStream stderr = new PrintStream(args[args.length-2]);
		System.setErr(stderr);
	} catch (FileNotFoundException e2) {
		// TODO Auto-generated catch block
		e2.printStackTrace();
	}
	try {
		PrintStream stdout = new PrintStream(args[args.length-3]);
		System.setOut(stdout);
	} catch (FileNotFoundException e2) {
		// TODO Auto-generated catch block
		e2.printStackTrace();
	}
	
	List<Class<?>> test_classes = new ArrayList<Class<?>>(); //List of loaded classes
	List<String> class_names = new ArrayList<String>(); 
	String[] jar_pathes = new String[args.length-3];
	System.arraycopy(args, 0, jar_pathes, 0, args.length-3);
	
	for (String jar_path : jar_pathes) {	
		try {
			JarFile jarFile = new java.util.jar.JarFile(jar_path);
			Enumeration<JarEntry> jar_entries_enum = jarFile.entries();
			
			URL[] urls = { new URL("jar:file:" + jar_pathes[0]+"!/") };
			URLClassLoader cl = URLClassLoader.newInstance(urls);
			
			while (jar_entries_enum.hasMoreElements()) {
		        JarEntry jar_entry = (JarEntry) jar_entries_enum.nextElement();
		        if(jar_entry.isDirectory() || !jar_entry.getName().endsWith(".class")) {
		        	continue;
		        }

			    String className = jar_entry.getName().substring(0,jar_entry.getName().length()-6); //-6 == len(".class")
			    className = className.replace('/', '.');
			    
			    Class<?> c = cl.loadClass(className);
			    System.err.println("TestCase.class.isAssignableFrom("+ c.getCanonicalName() + ") == " + TestCase.class.isAssignableFrom(c));
			    System.err.println("has_annotations("+ c.getCanonicalName() + ") == " + has_annotations(c));
			    
			    if (TestCase.class.isAssignableFrom(c) || has_annotations(c)){ 
			    	test_classes.add(c);
			    	System.err.println("class added to tests: " + c.getCanonicalName());
			    	class_names.add(className);
				}	
			}
			jarFile.close();
		} catch (IOException e) {
			System.err.println(jar_path);
			e.printStackTrace();
		} catch (ClassNotFoundException e1) {
			System.err.println(jar_path);
			e1.printStackTrace();
		}
		}
	
	if (test_classes.isEmpty())
	{
		throw new RuntimeException("Nothing to test");
		//System.exit(1);
	}
	else{
		JUnitCore runner = new JUnitCore();
		CustomListener custom_listener = new CustomListener();
		try {
			custom_listener.reporter = new JTLReporter(args[args.length-1]);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		runner.addListener(custom_listener);
		runner.run(test_classes.toArray(new Class[test_classes.size()]));
		}
	}

}
