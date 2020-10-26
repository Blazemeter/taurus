using NUnit.Framework;

namespace NUnitTests
{
    [TestFixture]
    public class NUnitTestExample
    {
        private int _numberToTest;
        
        [SetUp]
        public void SetUp()
        {
            _numberToTest = 5;
        }

        [Test]
        public void IsFive_ExistingVariable_ReturnFive()
        {
            Assert.AreEqual(5, _numberToTest);
        }
    }
}