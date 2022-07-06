namespace Springboard365.Core.UnitTest
{
    public class CustomAttributeUtilitiesSpecificationsFixture
    {
        public IAttributeUtilities UnderTest { get; set; }

        public void PerformTestSetup()
        {
            UnderTest = new AttributeUtilities();
        }
    }
}