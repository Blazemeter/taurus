using System;
using System.Threading.Tasks;
using NUnit.Engine;

namespace NUnitRunner.Models
{
    public static class Test
    {
        public static void RunTest(DateTime startTime, RunnerOptions options, TestEventListener testEventListener)
        {
            try
            {
                int iteration = 0;
                while (true)
                {
                    testEventListener.Runner.Run(testEventListener, TestFilter.Empty);
                    TimeSpan offset = DateTime.UtcNow - startTime;
                    bool durationStop = ((options.DurationLimit > 0) && (offset.TotalSeconds > options.DurationLimit));
                    bool iterationsStop = ((options.Iterations > 0) && (++iteration >= options.Iterations));
                    if  (durationStop || iterationsStop)
                    {
                        break;
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("EXCEPTION: {0}", e); ;
            }
        }
    }
}
