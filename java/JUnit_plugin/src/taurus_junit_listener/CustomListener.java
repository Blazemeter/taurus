package taurus_junit_listener;

import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;
import java.lang.System;

public class CustomListener extends RunListener{
	private String success;
	public JTLReporter reporter;
	private long timestamp;
	private String trace = "";
	private String message = "";
	private int total_tests = 0;
	private int success_tests = 0;
	private String log_pattern = "%s.%s,Total:%d Pass:%d Failed:%d\n";
	private int responseCode;
	
	public void testRunFinished(Result result) throws java.lang.Exception
	{
		reporter.close();
	}
	
	public void testStarted(Description description) throws Exception
	{	
		success = "true";
		trace = "";
		message = "";
		timestamp = System.currentTimeMillis();
		responseCode = 200;
		System.out.printf(log_pattern, description.getClassName(), description.getMethodName(), total_tests + 1, success_tests, total_tests - success_tests);
	}
	
	public void testFinished(Description description) throws java.lang.Exception
	{
		long elapsed = System.currentTimeMillis() - timestamp;
		String label = description.getMethodName();
		String responseMessage;
		String threadName = description.getClassName() + "." + description.getMethodName();
		
		if (success == "true"){
			responseMessage = "OK";
			success = "true";
			success_tests += 1;
		}
		else {
			
			if (message == null){
				message = "";
			}
			
			responseMessage = '"' + "--TRACE: " + trace.replace('"', '\'') + "\n" + "--MESSAGE: " + message.replace('"', '\'') + '"';
			success = "false";
		}
		total_tests += 1;
		
		reporter.report(timestamp, elapsed, label, responseCode, responseMessage, threadName, success);
	}
	
	public void testFailure(Failure failure) throws java.lang.Exception
	{
		success = "false";
		trace = failure.getTrace();
		message = failure.getMessage();
		responseCode = 500;
	}
	
	public void testAssumptionFailure(Failure failure){
		
		responseCode = 400;
		message = failure.getMessage();
	}
	
	public void testIgnored(Description description) throws java.lang.Exception
	{
		success = "false";
		responseCode = 300;
	}
	
}
