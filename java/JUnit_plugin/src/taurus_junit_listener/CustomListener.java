package taurus_junit_listener;

import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;
import java.lang.System;

public class CustomListener extends RunListener{
	private Boolean current_test_is_ok = false;
	public Reporter reporter;
	private long test_started;
	private String cur_class_name = "";
	private String trace = "";
	private String message = "";
	
	public void testRunStarted(Description description)	throws java.lang.Exception
	{
		//reporter.writeln("--BEGIN");
		//reporter.flush();
	}
	
	public void testRunFinished(Result result) throws java.lang.Exception
	{
		reporter.flush();
		reporter.close();
		
	}
	
	public void testStarted(Description description) throws java.lang.Exception
	{
		if (cur_class_name != description.getClassName()){
			cur_class_name = description.getClassName();
		}
		current_test_is_ok = true;
		trace = "";
		message = "";
		test_started = System.currentTimeMillis();
		reporter.writeln("--TIMESTAMP: " + test_started);
		reporter.writeln("--MODULE: "+ cur_class_name);
		reporter.writeln("--RUN: " + description.getClassName() + "." + description.getMethodName());
	}
	
	public void testFinished(Description description) throws java.lang.Exception
	{
		if (current_test_is_ok == true){
			reporter.writeln("--RESULT: OK");
		}
		else {
			reporter.writeln("--RESULT: FAILED");
			reporter.writeln("--TRACE: " + trace);
			reporter.writeln("--MESSAGE: " + message);
		}
		reporter.writeln("--TIME: " + (System.currentTimeMillis() - test_started));
	}
	
	public void testFailure(Failure failure) throws java.lang.Exception
	{
		//System.out.println("Execution of test case failed : "+ failure.getTrace());
		current_test_is_ok = false;
		trace = failure.getTrace();
		message = failure.getMessage();
	}
	
	public void testIgnored(Description description) throws java.lang.Exception
	{
		reporter.writeln("--RESULT: SKIPPED");
	}
	
}
