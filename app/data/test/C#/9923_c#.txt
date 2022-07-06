using System.Linq;

namespace DD.CBU.Compute.Api.Contracts.Network20
{

    public partial class ImageSataControllerType
    {
        [System.Xml.Serialization.XmlIgnoreAttribute()]
        public ImageSataDeviceType[] device => Items?.OfType<ImageSataDeviceType>().ToArray();

        /// <remarks/>
        [System.Xml.Serialization.XmlIgnoreAttribute()]
        public ImageSataDiskType[] disk => Items?.OfType<ImageSataDiskType>().ToArray();
    }

    public partial class ImageIdeControllerType
    {
        [System.Xml.Serialization.XmlIgnoreAttribute()]
        public ImageIdeDeviceType[] device => Items?.OfType<ImageIdeDeviceType>().ToArray();

        /// <remarks/>
        [System.Xml.Serialization.XmlIgnoreAttribute()]
        public ImageIdeDiskType[] disk => Items?.OfType<ImageIdeDiskType>().ToArray();
    }
}