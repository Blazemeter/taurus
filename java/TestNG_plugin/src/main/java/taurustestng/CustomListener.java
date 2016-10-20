package taurustestng;

import java.util.logging.Logger;

import org.testng.ITestContext;
import org.testng.ITestResult;
import org.testng.TestListenerAdapter;
import org.testng.internal.IResultListener;

public class CustomListener implements IResultListener {
    private static final Logger log = Logger.getLogger(CustomListener.class.getName());
    private Sample pendingSample;
    private TaurusReporter reporter;
    private long started = 0;

    private long testCount = 0;
    private long successCount = 0;
    private long failedCount = 0;
    private final static String report_tmpl = "%s.%s,Total:%d Passed:%d Failed:%d\n";

    public CustomListener() {
        super();
        // this.reporter = reporter;
    }

    public void reportToStdout() {
        System.out.printf(report_tmpl,
                pendingSample.getDescription(),
                pendingSample.getSuite(),
                testCount,
                successCount,
                failedCount);
    }

    public void onTestStart(ITestResult tr) {
        pendingSample = new Sample();
        pendingSample.setDescription(tr.getTestClass().getRealClass().getSimpleName());
        pendingSample.setSuite(tr.getMethod().getMethodName());
        testCount += 1;
    }

    public void onTestFailure(ITestResult tr) {
        failedCount += 1;
        pendingSample.setStatus("FAILED");
        reportToStdout();
    }

    public void onTestSkipped(ITestResult tr) {
        pendingSample.setStatus("SKIPPED");
        reportToStdout();
    }

    public void onTestSuccess(ITestResult tr) {
        pendingSample.setStatus("SUCCESS");
        successCount += 1;
        reportToStdout();
    }

    public void onTestFailedButWithinSuccessPercentage(ITestResult var1) {

    }

    public void onStart(ITestContext var1) {
        System.out.println("onStart");
    }

    public void onFinish(ITestContext var1) {
        System.out.println("onFinish");
    }

    public void onConfigurationFailure(ITestResult tr) {

    }

    public void onConfigurationSkip(ITestResult tr) {

    }

    public void onConfigurationSuccess(ITestResult tr) {
        System.out.println("onConfigurationSuccess");
    }

}


//public class CustomListener extends RunListener {
//
//    private static final Logger log = Logger.getLogger(CustomListener.class.getName());
//    private Sample pendingSample;
//    private TaurusReporter reporter;
//    private long started = 0;
//
//    private long testCount = 0;
//    private long failedCount = 0;
//    private final static String report_tmpl = "%s.%s,Total:%d Passed:%d Failed:%d\n";
//
//    public CustomListener(TaurusReporter reporter) {
//        super();
//        this.reporter = reporter;
//    }
//
//    public void testRunStarted(Description description) {
//        log.info("Run Started: " + description.getDisplayName());
//    }
//
//    public void testRunFinished(Result result) throws java.lang.Exception {
//        log.info("Run Finished, successful=" + result.wasSuccessful() + ", run count=" + result.getRunCount());
//    }
//
//    public void testStarted(Description description) throws Exception {
//        log.info(String.format("started %s", description.getDisplayName()));
//        started = System.currentTimeMillis();
//        pendingSample = new Sample();
//        pendingSample.setLabel(description.getMethodName());
//        pendingSample.setSuite(description.getClassName());
//        pendingSample.setFullName(description.getClassName() + "." + description.getMethodName());
//        testCount += 1;
//    }
//
//    public void testFinished(Description description) throws java.lang.Exception {
//        log.info(String.format("finished %s", description.getDisplayName()));
//        double duration = (System.currentTimeMillis() - started) / 1000.0;
//        pendingSample.setDuration(duration);
//        reporter.writeSample(pendingSample);
//
//        if (!pendingSample.isSuccessful()) {
//            failedCount += 1;
//        }
//        pendingSample = null;
//        System.out.printf(report_tmpl,
//                description.getClassName(),
//                description.getMethodName(),
//                testCount,
//                testCount - failedCount,
//                failedCount);
//    }
//
//    public void testFailure(Failure failure) throws java.lang.Exception {
//        log.severe(String.format("failed %s", failure.toString()));
//        pendingSample.setStatus(Sample.STATUS_BROKEN);
//        String exceptionName = failure.getException().getClass().getName();
//        pendingSample.setErrorMessage(exceptionName + ": " + failure.getMessage());
//        pendingSample.setErrorTrace(Utils.getStackTrace(failure.getException()));
//    }
//
//    public void testAssumptionFailure(Failure failure) {
//        log.severe(String.format("assert failed %s", failure.toString()));
//        pendingSample.setStatus(Sample.STATUS_FAILED);
//        String exceptionName = failure.getException().getClass().getName();
//        pendingSample.setErrorMessage(exceptionName + ": " + failure.getMessage());
//        pendingSample.setErrorTrace(Utils.getStackTrace(failure.getException()));
//    }
//
//    public void testIgnored(Description description) throws java.lang.Exception {
//        log.warning(String.format("ignored %s", description.getDisplayName()));
//        pendingSample.setStatus(Sample.STATUS_SKIPPED);
//        pendingSample.setErrorMessage(description.getDisplayName());
//    }
//
//}
