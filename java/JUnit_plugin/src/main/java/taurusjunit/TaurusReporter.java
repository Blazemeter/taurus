package taurusjunit;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Logger;

import org.json.*;

public class TaurusReporter {

    private FileWriter outStream;
    private static final Logger log = Logger.getLogger(TaurusReporter.class.getName());

    public TaurusReporter(String file_name) {
        log.info("LDJSON file: " + file_name);
        try {
            outStream = new FileWriter(new File(file_name));
        } catch (IOException e) {
            throw new RuntimeException("Failed to open file " + file_name, e);
        }
    }

    public void writeSample(Sample sample) throws Exception {
        try {
            JSONObject obj = new JSONObject();
            obj.put("label", sample.getLabel());
            obj.put("start_time", sample.getStartTime());
            obj.put("duration", sample.getDuration());
            obj.put("status", sample.getStatus());
            obj.put("error_msg", sample.getErrorMessage());
            obj.put("error_trace", sample.getErrorTrace());
            obj.put("file", sample.getFile());
            obj.put("full_name", sample.getFullName());
            obj.put("description", sample.getDescription());
            outStream.write(obj.toString() + "\n");
            outStream.flush();
        } catch (Exception e) {
            log.severe("Ex: " + Utils.getStackTrace(e));
        }
    }

    public void close() throws Exception {
        log.info("Closing LDJSON reporter stream");
        outStream.close();
    }
}
