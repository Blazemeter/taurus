using NUnit.Framework;
using NUnit.Framework.Legacy;

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
            ClassicAssert.AreEqual(5, _numberToTest);
        }
    }
}