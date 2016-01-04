package taurusjunit;

import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

import java.util.logging.Logger;

public class CustomListener extends RunListener {

    private static final Logger log = Logger.getLogger(CustomListener.class.getName());
    private Sample pendingSample;
    private JTLReporter reporter;
    private JTLErrorReporter err_reporter;
    private long started = 0;

    private long test_count = 0;
    private long failed_count = 0;
    private final static String report_tmpl = "%s.%s,Total:%d Pass:%d Failed:%d\n";

    public CustomListener(JTLReporter jtlReporter, JTLErrorReporter jtlErrorReporter) {
        super();
        reporter = jtlReporter;
        err_reporter = jtlErrorReporter;
    }

    public void testRunStarted(Description description) {
        log.info("Run Started: " + description.getDisplayName());
    }

    public void testRunFinished(Result result) throws java.lang.Exception {
        log.info("Run Finished, successful=" + result.wasSuccessful() + ", run count=" + result.getRunCount());
        reporter.close();
        err_reporter.close();
    }

    public void testStarted(Description description) throws Exception {
        log.info(String.format("started %s", description.getDisplayName()));
        started = System.currentTimeMillis();
        pendingSample = new Sample();
        pendingSample.setLabel(description.getMethodName());
        pendingSample.setThreadName(description.getClassName());
        test_count += 1;
    }

    public void testFinished(Description description) throws java.lang.Exception {
        log.info(String.format("finished %s", description.getDisplayName()));
        pendingSample.setElapsed(System.currentTimeMillis() - started);
        reporter.writeSample(pendingSample);

        if (pendingSample.getResponseCode() != 200) {
            err_reporter.add_sample(pendingSample);
            failed_count += 1;
        }
        pendingSample = null;
        System.out.printf(report_tmpl,
                description.getClassName(),
                description.getMethodName(),
                test_count,
                test_count - failed_count,
                failed_count);
    }

    public void testFailure(Failure failure) throws java.lang.Exception {
        log.severe(String.format("failed %s", failure.toString()));
        pendingSample.setSuccess(false);
        pendingSample.setTrace(failure.getMessage());
        pendingSample.setMessage(failure.getException().getClass().getName());
        pendingSample.setResponseCode(500);
    }

    public void testAssumptionFailure(Failure failure) {
        log.warning(String.format("failed assert %s", failure.getDescription()));
        pendingSample.setSuccess(false);
        pendingSample.setTrace(failure.getMessage());
        pendingSample.setMessage(failure.getException().getClass().getName());
        pendingSample.setResponseCode(400);
    }

    public void testIgnored(Description description) throws java.lang.Exception {
        log.warning(String.format("ignored %s", description.getDisplayName()));
        pendingSample.setSuccess(false);
        pendingSample.setMessage(description.getDisplayName());
        pendingSample.setResponseCode(300);
    }

}
