using Xunit;

namespace XUnitExample
{
    public class UnitTest1
    {
        [Fact(DisplayName = "AssertThatOneEquals1_Success")]       
        public void AssertThatOneEquals1_Success()
        {
            Assert.Equal(1,1);
        }


        [Fact(DisplayName = "AssertThatOneEquals1_Success")]
        public void AssertThatTwoEquals2_Success()
        {
            Assert.Equal(2, 2);
        }
    }
}
