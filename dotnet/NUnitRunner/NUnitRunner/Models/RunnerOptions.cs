namespace NUnitRunner.Models
{
    public class RunnerOptions
    {
        public RunnerOptions()
        {
            ReportFile = "report.ldjson";
            Iterations = 0;
            DurationLimit = 0;
            Concurrency = 0;
            RampUp = 0;
            TargetAssembly = null;
            ShouldShowHelp = false;
        }

        public string ReportFile { get; set; }

        public int Iterations { get; set; }

        public int DurationLimit { get; set; }

        public int Concurrency { get; set; }

        public int RampUp { get; set; }

        public string TargetAssembly { get; set; }

        public bool ShouldShowHelp { get; set; }
    }
}
