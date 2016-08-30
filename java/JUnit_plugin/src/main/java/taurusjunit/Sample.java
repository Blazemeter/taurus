package taurusjunit;

public class Sample {
    public static final String STATUS_PASSED = "PASSED";
    public static final String STATUS_FAILED = "FAILED";
    public static final String STATUS_BROKEN = "BROKEN";
    public static final String STATUS_SKIPPED = "SKIPPED";

    private final long startTime = System.currentTimeMillis() / 1000;
    private double duration = 0;
    private String status = STATUS_PASSED;
    private String label = "";
    private String suite = "";
    private String file = "";
    private String fullName = "";
    private String errorMessage = "";
    private String errorTrace = "";
    private String description = "";

    public boolean isSuccessful() {
        return this.status.equals(STATUS_PASSED);
    }

    @Override
    public String toString() {
        return String.format("%d: %s - %s", startTime, label, status);
    }

    public long getStartTime() {
        return startTime;
    }

    public double getDuration() {
        return duration;
    }

    public void setDuration(double duration) {
        this.duration = duration;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String message) {
        this.errorMessage = message == null ? "" : message;
    }

    public String getErrorTrace() {
        return errorTrace;
    }

    public void setErrorTrace(String trace) {
        this.errorTrace = trace == null ? "" : trace;
    }

    public String getFile() {
        return file;
    }

    public void setFile(String file) {
        this.file = file == null ? "" : file;
    }

    public String getFullName() {
        return fullName;
    }

    public void setFullName(String fullName) {
        this.fullName = fullName == null ? "" : fullName;

    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description == null ? "" : description;
    }

    public String getSuite() {
        return suite;
    }

    public void setSuite(String suite) {
        this.suite = suite == null ? "" : suite;
    }
}

