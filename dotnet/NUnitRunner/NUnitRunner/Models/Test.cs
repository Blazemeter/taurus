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
                for (int t = 0; t < options.Iterations; t++)
                {
                    testEventListener.Runner.Run(testEventListener, TestFilter.Empty);
                    TimeSpan offset = DateTime.UtcNow - startTime;
                    if (options.DurationLimit > 0 && offset.TotalSeconds > options.DurationLimit)
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
