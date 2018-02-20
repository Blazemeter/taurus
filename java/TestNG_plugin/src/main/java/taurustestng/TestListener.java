package taurustestng;

import java.util.logging.Logger;

import org.testng.ITestContext;
import org.testng.ITestResult;
import org.testng.internal.IResultListener;

public class TestListener implements IResultListener {
    private static final Logger log = Logger.getLogger(TestListener.class.getName());
    private TaurusReporter reporter;

    private long testCount = 0;
    private long successCount = 0;
    private long failedCount = 0;
    private final static String report_tmpl = "%s.%s,Total:%d Passed:%d Failed:%d\n";

    public TestListener(TaurusReporter reporter) {
        super();
        this.reporter = reporter;
    }

    private synchronized void reportToStdout(Sample sample) {
        System.out.printf(report_tmpl,
                sample.getSuite(),
                sample.getLabel(),
                testCount,
                successCount,
                failedCount);
    }

    private Sample captureSample(ITestResult tr) {
        Sample sample = new Sample();
        Class testClass = tr.getTestClass().getRealClass();
        String methodName = tr.getMethod().getMethodName();
        sample.setLabel(methodName);
        sample.setSuite(testClass.getSimpleName());
        sample.setFullName(testClass.getName() + "." + methodName);
        sample.setStartTime(tr.getStartMillis() / 1000);
        double durationMs = tr.getEndMillis() - tr.getStartMillis();
        sample.setDuration(durationMs / 1000.0);
        switch (tr.getStatus()) {
            case ITestResult.FAILURE:
                sample.setStatus(Sample.STATUS_FAILED);
                Throwable failure = tr.getThrowable();
                sample.setErrorMessage(failure.getClass().getSimpleName() + ": " + failure.getMessage());
                sample.setErrorTrace(Utils.getStackTrace(failure));
                break;
            case ITestResult.SKIP:
                sample.setStatus(Sample.STATUS_SKIPPED);
                break;
            case ITestResult.SUCCESS:
                sample.setStatus(Sample.STATUS_PASSED);
                break;
            default:
                log.warning(String.format("Unknown test status: %d", tr.getStatus()));
                break;
        }

        return sample;
    }

    public void onTestStart(ITestResult tr) {
        testCount += 1;
    }

    public void onTestFailure(ITestResult tr) {
        failedCount += 1;
        Sample sample = captureSample(tr);
        reportToStdout(sample);
        reporter.writeSample(sample);
    }

    public void onTestSkipped(ITestResult tr) {
        Sample sample = captureSample(tr);
        reportToStdout(sample);
        reporter.writeSample(sample);
    }

    public void onTestSuccess(ITestResult tr) {
        successCount += 1;
        Sample sample = captureSample(tr);
        reportToStdout(sample);
        reporter.writeSample(sample);
    }

    public void onTestFailedButWithinSuccessPercentage(ITestResult var1) {
        log.info("onTestFailedButWithinSuccessPercentage");
    }

    public void onStart(ITestContext var1) {
        log.info("onStart");
    }

    public void onFinish(ITestContext var1) {
        log.info("onFinish");
    }

    public void onConfigurationFailure(ITestResult tr) {
        log.info("onConfigurationFailure: " + Utils.getStackTrace(tr.getThrowable()));
        failedCount += 1;
        Sample sample = captureSample(tr);
        reportToStdout(sample);
        reporter.writeSample(sample);
    }

    public void onConfigurationSkip(ITestResult tr) {
        log.info("onConfigurationSkip");
    }

    public void onConfigurationSuccess(ITestResult tr) {
        log.info("onConfigurationSuccess");
    }

}