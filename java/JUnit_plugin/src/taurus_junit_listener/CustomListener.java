package taurus_junit_listener;

import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

import java.lang.System;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.Handler;
import java.util.logging.ConsoleHandler;

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
	private Logger log = Logger.getLogger(CustomListener.class.getName());
	
	public void testRunStarted(Description description){
		log.setUseParentHandlers(false);
		log.setLevel(Level.FINER);
		Handler hnd = new ConsoleHandler();
		hnd.setLevel(Level.FINER);
		log.addHandler(hnd);
	}
	
	public void testRunFinished(Result result) throws java.lang.Exception
	{
		if (total_tests == 0){
			System.err.println("total_tests = 0");
			//throw new RuntimeException("total_tests = 0.");
		}
		System.err.println("result was successful? " + result.wasSuccessful());
		System.err.println("result run count " + result.getRunCount());
		reporter.close();
		
	}
	
	public void testStarted(Description description) throws Exception
	{	
		log.info(String.format("started %s", description.getDisplayName()));
		success = "true";
		trace = "";
		message = "";
		timestamp = System.currentTimeMillis();
		responseCode = 200;
		System.out.printf(log_pattern, description.getClassName(), description.getMethodName(), total_tests + 1, success_tests, total_tests - success_tests);
		log.info(String.format("after started %s", description.getDisplayName()));
	}
	
	public void testFinished(Description description) throws java.lang.Exception
	{
		long elapsed = System.currentTimeMillis() - timestamp;
		String label = description.getMethodName();
		String responseMessage;
		String threadName = description.getClassName() + "." + description.getMethodName();
		log.info(String.format("finished %s", description.getDisplayName()));
		
		if (success == "true"){
			responseMessage = "OK";
			success = "true";
			success_tests += 1;
		}
		else {
			
			if (message == null){
				log.warning("null message");
				System.err.print("");
				message = "";
			}
			log.info(trace);
			log.info(message);
			
			responseMessage = '"' + "--TRACE: " + trace.replace('"', '\'') + "\n" + "--MESSAGE: " + message.replace('"', '\'') + '"';
			log.info(String.format("responseMessage: %s", responseMessage));
			success = "false";
		}
		total_tests += 1;
		log.info("reporter reporting");
		reporter.report(timestamp, elapsed, label, responseCode, responseMessage, threadName, success);
		log.info("done reporting");
	}
	
	public void testFailure(Failure failure) throws java.lang.Exception
	{
		success = "false";
		trace = failure.getTrace();
		message = failure.getMessage();
		if (message == null){
			
		}
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
