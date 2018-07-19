namespace XUnitRunner
{
    public class RunnerOptions
    {
        public int durationLimit = 0;
        public int iterations = 0;
        public string reportFile = "report.ldjson";
        public bool shouldShowHelp = false;
        public string targetAssembly = null;
    }
}