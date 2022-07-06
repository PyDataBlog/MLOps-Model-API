using System;

namespace CloudFormationCs.Resources.Route53
{
    /// <summary>
    /// http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-route53-recordsetgroup.html
    /// </summary>
    public class RecordSetGroup : Resource
    {
        [Required(RequiredAttribute.RequirementTypes.Conditional)]
        public String HostedZoneId { get; set; }

        [Required(RequiredAttribute.RequirementTypes.Conditional)]
        public String HostedZoneName { get; set; }

        [Required(true)]
        public RecordSet[] RecordSets { get; set; }

        [Required(false)]
        public String Comment { get; set; }

        public RecordSetGroup()
            : base()
        {
        }

        public RecordSetGroup(StringOrEnum resourceIdentifier)
            : base(resourceIdentifier)
        {
        }
    }
}
