using Xunit;

namespace XUnitTests
{
    public class XUnitTestExample
    {
        private readonly int _numberToTest;
        
        public XUnitTestExample()
        {
            _numberToTest = 5;
        }

        [Fact]
        public void IsFive_ExistingVariable_ReturnFive()
        {
            Assert.Equal(5, _numberToTest);
        }
    }
}