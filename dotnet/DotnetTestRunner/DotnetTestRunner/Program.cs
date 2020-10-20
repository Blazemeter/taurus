using System.CommandLine;
using System.Threading.Tasks;
using DotnetTestRunner.Services.NUnit;
using DotnetTestRunner.Services.xUnit;

namespace DotnetTestRunner
{
    public static class Program
    {
        public static async Task Main(string[] args)
        {
            var command = new RootCommand(".NET test runner for Taurus")
            {
                NUnitTestRunner.GetNUnitCommand(),
                XUnitTestRunner.GetNUnitCommand()
            };

            await command.InvokeAsync(args);
        }
    }
}
