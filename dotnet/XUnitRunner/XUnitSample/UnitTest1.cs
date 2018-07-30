using Xunit;

namespace XUnitExample
{
    public class UnitTest1
    {
        [Fact(DisplayName = "AssertThatOneEquals1_Success")]
        public void AssertThatOneEquals1_Success()
        {
            Assert.Equal(1, 1);
        }


        [Fact(DisplayName = "AssertThatTwoEquals2_Failure")]
        public void AssertThatTwoEquals2_Failure()
        {
            Assert.Equal(2, 4);
        }
    }
}
