package taurus_junit_listener;

import java.io.File;
import java.io.FileWriter;
import java.util.logging.Logger;

public class JTLReporter {
	
	private FileWriter out_stream;
	private String jtl_record_pattern = "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n";
	private Logger log = Logger.getLogger(CustomListener.class.getName());
	
	public JTLReporter(String file_name) throws Exception
	{
		out_stream = new FileWriter(new File(file_name));
		out_stream.write("timeStamp,elapsed,label,responseCode,responseMessage,threadName,success,grpThreads,allThreads,Latency,Connect\n");
		log.info("created, header");
	}
	
	public void report(long timestamp, long elapsed, String label, int responseCode, String responseMessage, String threadName, String success) throws Exception{
		String fmt_msg = String.format(jtl_record_pattern, timestamp, elapsed, label, responseCode, responseMessage, threadName, success, 1, 1, 0, 0);
		out_stream.write(fmt_msg);
		out_stream.flush();
	}
	public void close() throws Exception{
		out_stream.close();
		log.info("reporter stream closed.");
	}
}
