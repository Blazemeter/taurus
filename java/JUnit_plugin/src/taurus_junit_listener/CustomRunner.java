package taurus_junit_listener;

import org.junit.runner.JUnitCore;

import junit.framework.TestCase;
import taurus_junit_listener.CustomListener;

import java.io.IOException;
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
	
	List<Class<?>> test_classes = new ArrayList<Class<?>>(); //List of loaded classes
	List<String> class_names = new ArrayList<String>(); 
	String[] jar_pathes = new String[args.length -1];
	System.arraycopy(args, 0, jar_pathes, 0, args.length-1);
	
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
			    if (TestCase.class.isAssignableFrom(c) || has_annotations(c)){ 
			    	test_classes.add(c);
			    	class_names.add(className);
				}	
			}
			jarFile.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (ClassNotFoundException e1) {
			e1.printStackTrace();
			System.exit(1);
		}
		}
	
	if (test_classes.isEmpty())
	{
		System.err.println("There is nothing to test.");
		System.exit(1);
	}
	else{
		//System.out.println(Arrays.toString(class_names.toArray()));
		
	JUnitCore runner = new JUnitCore();
	CustomListener custom_listener = new CustomListener();
	try {
		custom_listener.reporter = new JTLReporter(args[args.length-1]);
	} catch (Exception e) {
		e.printStackTrace();
		System.exit(1);
	}
	runner.addListener(custom_listener);
	runner.run(test_classes.toArray(new Class[test_classes.size()]));
		}
	}

}
