package taurusjunit;

import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

import java.util.logging.Logger;

public class CustomListener extends RunListener {

    private static final Logger log = Logger.getLogger(CustomListener.class.getName());
    private Sample pendingSample;
    private TaurusReporter reporter;
    private long started = 0;

    private long testCount = 0;
    private long failedCount = 0;
    private final static String report_tmpl = "%s.%s,Total:%d Passed:%d Failed:%d\n";

    public CustomListener(TaurusReporter reporter) {
        super();
        this.reporter = reporter;
    }

    public void testRunStarted(Description description) {
        log.info("Run Started: " + description.getDisplayName());
    }

    public void testRunFinished(Result result) throws java.lang.Exception {
        log.info("Run Finished, successful=" + result.wasSuccessful() + ", run count=" + result.getRunCount());
    }

    public void testStarted(Description description) throws Exception {
        log.info(String.format("started %s", description.getDisplayName()));
        started = System.currentTimeMillis();
        pendingSample = new Sample();
        pendingSample.setLabel(description.getMethodName());
        pendingSample.setSuite(description.getClassName());
        pendingSample.setFullName(description.getClassName() + "." + description.getMethodName());
        testCount += 1;
    }

    public void testFinished(Description description) throws java.lang.Exception {
        log.info(String.format("finished %s", description.getDisplayName()));
        double duration = (System.currentTimeMillis() - started) / 1000.0;
        pendingSample.setDuration(duration);
        reporter.writeSample(pendingSample);

        if (!pendingSample.isSuccessful()) {
            failedCount += 1;
        }
        pendingSample = null;
        System.out.printf(report_tmpl,
                description.getClassName(),
                description.getMethodName(),
                testCount,
                testCount - failedCount,
                failedCount);
    }

    public void testFailure(Failure failure) throws java.lang.Exception {
        log.severe(String.format("failed %s", failure.toString()));
        pendingSample.setStatus(Sample.STATUS_BROKEN);
        String exceptionName = failure.getException().getClass().getName();
        pendingSample.setErrorMessage(exceptionName + ": " + failure.getMessage());
        pendingSample.setErrorTrace(Utils.getStackTrace(failure.getException()));
    }

    public void testAssumptionFailure(Failure failure) {
        log.severe(String.format("assert failed %s", failure.toString()));
        pendingSample.setStatus(Sample.STATUS_FAILED);
        String exceptionName = failure.getException().getClass().getName();
        pendingSample.setErrorMessage(exceptionName + ": " + failure.getMessage());
        pendingSample.setErrorTrace(Utils.getStackTrace(failure.getException()));
    }

    public void testIgnored(Description description) throws java.lang.Exception {
        log.warning(String.format("ignored %s", description.getDisplayName()));
        pendingSample.setStatus(Sample.STATUS_SKIPPED);
        pendingSample.setErrorMessage(description.getDisplayName());
    }

}
