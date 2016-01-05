package taurusjunit;

public class Sample {
    private boolean success = true;
    private String trace = "";
    private String message = "OK";
    private int responseCode = 200;
    private long elapsed = 0;
    private final long timestamp = System.currentTimeMillis();
    private String label = "";
    private String threadName = "";

    public boolean isSuccessful() {
        return success;
    }

    @Override
    public String toString() {
        return timestamp + " " + responseCode + " " + message;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public void setElapsed(long elapsed) {
        this.elapsed = elapsed;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public void setThreadName(String threadName) {
        this.threadName = threadName;
    }

    public void setTrace(String trace) {
        this.trace = trace == null ? "" : trace;
    }

    public String getTrace() {
        return trace;
    }

    public void setMessage(String message) {
        this.message = message == null ? "" : message;
    }

    public void setResponseCode(int responseCode) {
        this.responseCode = responseCode;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public long getElapsed() {
        return elapsed;
    }

    public String getLabel() {
        return label;
    }

    public int getResponseCode() {
        return responseCode;
    }

    public String getResponseMessage() {
        return message;
    }

    public String getThreadName() {
        return threadName;
    }
}

