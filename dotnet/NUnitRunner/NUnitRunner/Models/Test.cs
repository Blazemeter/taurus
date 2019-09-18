using System;
using System.Threading.Tasks;
using NUnit.Engine;

namespace NUnitRunner.Models
{
    public static class Test
    {
        public static async Task RunTest(DateTime startTime, RunnerOptions options, TestEventListener testEventListener)
        {
            try
            {
                for (int t = 0; t < options.Iterations; t++)
                {
                    await Task.Run(() => testEventListener.Runner.Run(testEventListener, TestFilter.Empty));
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
