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

    public CustomListener(JTLReporter jtlReporter, JTLErrorReporter jtlErrorReporter) {
        super();
        reporter = jtlReporter;
        err_reporter = jtlErrorReporter;
    }

    public void testRunStarted(Description description) {
        log.info("Run Started: " + description);
    }

    public void testRunFinished(Result result) throws java.lang.Exception {
        log.info("Run Finished, successful=" + result.wasSuccessful() + ", run count=" + result.getRunCount());
        reporter.close();
        err_reporter.save();
    }

    public void testStarted(Description description) throws Exception {
        log.info(String.format("started %s", description.getDisplayName()));
        started = System.currentTimeMillis();
        pendingSample = new Sample();
    }

    public void testFinished(Description description) throws java.lang.Exception {
        log.info(String.format("finished %s", description.getDisplayName()));
        pendingSample.setElapsed(System.currentTimeMillis() - started);
        pendingSample.setLabel(description.getMethodName());
        pendingSample.setThreadName(description.getClassName() + "." + description.getMethodName());
        reporter.writeSample(pendingSample);
        pendingSample = null;
    }


    public void testFailure(Failure failure) throws java.lang.Exception {
        log.severe(String.format("failed %s", failure.toString()));

        Sample sample;
        if (pendingSample == null) {
            sample = new Sample();
        } else {
            sample = pendingSample;
        }
        sample.setSuccess(false);
        sample.setTrace(failure.getTrace());
        sample.setMessage(failure.getMessage());
        sample.setResponseCode(500);
        err_reporter.add_sample(sample);
        
        if (pendingSample == null) {
            reporter.writeSample(sample);
        }
    }

    public void testAssumptionFailure(Failure failure) {
        log.warning(String.format("failed assert %s", failure.getDescription()));
        pendingSample.setSuccess(false);
        pendingSample.setMessage(failure.getMessage());
        pendingSample.setResponseCode(400);
        err_reporter.add_sample(pendingSample);
    }

    public void testIgnored(Description description) throws java.lang.Exception {
        log.warning(String.format("ignored %s", description.getDisplayName()));
        pendingSample.setSuccess(false);
        pendingSample.setMessage(description.getDisplayName());
        pendingSample.setResponseCode(300);
        err_reporter.add_sample(pendingSample);
    }

}
