using System.Linq;
using Xunit;

namespace Farity.Tests
{
    public class FilterTests
    {
        [Fact]
        public void FilterFiltersElementsInTheList()
        {
            var expected = new[] {1, 2, 3};
            var actual = F.Filter(x => x < 4, new[] {1, 2, 3, 4, 5});
            Assert.Equal(expected, actual);
        }

        [Fact]
        public void FilterWorksWithTrueAndFalse()
        {
            var expected = new[] { 1, 2, 3, 4, 5 };
            var actual = F.Filter(F.True, new[] { 1, 2, 3, 4, 5 });
            Assert.Equal(expected, actual);
            expected = new int[] {};
            actual = F.Filter(F.False, new[] { 1, 2, 3, 4, 5 });
            Assert.Equal(expected, actual);
        }
    }
}