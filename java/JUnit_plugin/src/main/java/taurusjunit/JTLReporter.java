package taurusjunit;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Logger;

public class JTLReporter {

    public static final String HEADER = "timeStamp,elapsed,label,responseCode,responseMessage,threadName,success,grpThreads,allThreads,Latency,Connect\n";
    private static final String jtl_record_pattern = "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n";
    private FileWriter out_stream;
    private static final Logger log = Logger.getLogger(JTLReporter.class.getName());

    public JTLReporter(String file_name) {
        log.info("JTL file: " + file_name);
        try {
            out_stream = new FileWriter(new File(file_name));
            out_stream.write(HEADER);
            log.info("Wrote header");
        } catch (IOException e) {
            throw new RuntimeException("Failed to open file " + file_name, e);
        }
    }

    public void writeSample(Sample sample) throws Exception {
        try {
            String fmt_msg = String.format(jtl_record_pattern, sample.getTimestamp(), sample.getElapsed(),
                    sample.getLabel(), sample.getResponseCode(), sample.getResponseMessage(), sample.getThreadName(),
                    sample.isSuccessful() ? "true" : "false", 1, 1, 0, 0);
            out_stream.write(fmt_msg);
            out_stream.flush();
        } catch (Exception e) {
            log.severe("Ex: " + Utils.getStackTrace(e));
        }
    }

    public void close() throws Exception {
        log.info("Closing reporter stream");
        out_stream.close();
    }
}
