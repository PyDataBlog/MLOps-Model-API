using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Trafikverket.NET
{
    /// <summary>
    /// Timetable information, represents a single train at a location
    /// </summary>
    public class TrainAnnouncement
    {
        /// <summary>
        /// Get ObjectType as string
        /// </summary>
        public static string ObjectTypeName
        {
            get { return "TrainAnnouncement"; }
        }

        /// <summary>
        /// Get or set unique id for the activity
        /// </summary>
        [XmlElement("ActivityId")]
        public string ActivityId { get; set; }

        /// <summary>
        /// Get or set type of activity, either "Ankomst" or "Angang"
        /// </summary>
        [XmlElement("ActivityType")]
        public string ActivityType { get; set; }

        /// <summary>
        /// Get or set if the activity is advertised in the time table
        /// </summary>
        [XmlElement("Advertised")]
        public bool Advertised { get; set; }

        /// <summary>
        /// Get or set advertised time in time table
        /// </summary>
        [XmlElement("AdvertisedTimeAtLocation")]
        public DateTime AdvertisedTimeAtLocation { get; set; }

        /// <summary>
        /// Get or set announced train number
        /// </summary>
        [XmlElement("AdvertisedTrainIdent")]
        public string AdvertisedTrainIdent { get; set; }

        /// <summary>
        /// Get or set booking information, e.g. "Vagn 4 obokad"
        /// </summary>
        [XmlElement("Booking")]
        public List<string> Booking { get; set; }

        /// <summary>
        /// Get or set if the activity is cancelled
        /// </summary>
        [XmlElement("Canceled")]
        public bool Canceled { get; set; }

        /// <summary>
        /// Get or set if this data post has been deleted
        /// </summary>
        [XmlElement("Deleted")]
        public bool Deleted { get; set; }

        /// <summary>
        /// Get or set possible deviation texts, e.g. "Buss ersätter"
        /// </summary>
        [XmlElement("Deviation")]
        public List<string> Deviation { get; set; }

        /// <summary>
        /// Get or set estimated time for arrival or departure
        /// </summary>
        [XmlElement("EstimatedTimeAtLocation")]
        public DateTime? EstimatedTimeAtLocation { get; set; }

        /// <summary>
        /// Get or set if the estimated time is preliminary
        /// </summary>
        [XmlElement("EstimatedTimeIsPreliminary")]
        public bool EstimatedTimeIsPreliminary { get; set; }

        /// <summary>
        /// Get or set from stations, sorted by order and priority to be displayed
        /// </summary>
        [XmlElement("FromLocation")]
        public List<string> FromLocation { get; set; }

        /// <summary>
        /// Get or set name of traffic information owner
        /// </summary>
        [XmlElement("InformationOwner")]
        public string InformationOwner { get; set; }

        /// <summary>
        /// Get or set signature for the station
        /// </summary>
        [XmlElement("LocationSignature")]
        public string LocationSignature { get; set; }

        /// <summary>
        /// Get or set url to traffic owners mobile website
        /// </summary>
        [XmlElement("MobileWebLink")]
        public string MobileWebLink { get; set; }

        /// <summary>
        /// Get or set modified time for this data post
        /// </summary>
        [XmlElement("ModifiedTime")]
        public DateTime Modified { get; set; }

        /// <summary>
        /// Get or set other announcement text, e.g. "Trevlig resa!", "Ingen påstigning", etc.
        /// </summary>
        [XmlElement("OtherInformation")]
        public List<string> OtherInformation { get; set; }

        /// <summary>
        /// Get or set description of the train, e.g. "Tågkompaniet", "SJ", "InterCity" etc.
        /// </summary>
        [XmlElement("ProductInformation")]
        public List<string> ProductInformation { get; set; }

        /// <summary>
        /// Get or set announced departure date
        /// </summary>
        [XmlElement("ScheduledDepartureDateTime")]
        public DateTime? ScheduledDeparture { get; set; }

        /// <summary>
        /// Get or set additional product information, e.g. "Bistro" etc.
        /// </summary>
        [XmlElement("Service")]
        public List<string> Service { get; set; }

        /// <summary>
        /// Get or set when train arrived or departed
        /// </summary>
        [XmlElement("TimeAtLocation")]
        public DateTimeOffset TimeAtLocation { get; set; }

        /// <summary>
        /// Get or set to locations, in order after priority in which to be displayed
        /// </summary>
        [XmlElement("ToLocation")]
        public List<string> ToLocation { get; set; }

        /// <summary>
        /// Get or set track at the station
        /// </summary>
        [XmlElement("TrackAtLocation")]
        public string TrackAtLocation { get; set; }

        /// <summary>
        /// Get or set train compisition, e.g. "Vagnsordning 7, 6, 5, 4, 2, 1"
        /// </summary>
        [XmlElement("TrainComposition")]
        public List<string> TrainComposition { get; set; }

        /// <summary>
        /// Get or set type of traffic, values "Tåg", "Direktbuss", "Extrabuss", "Ersättningsbuss", or "Taxi".
        /// </summary>
        [XmlElement("TypeOfTraffic")]
        public string TypeOfTraffic { get; set; }

        /// <summary>
        /// Get or set url to traffic owners website
        /// </summary>
        [XmlElement("WebLink")]
        public string WebLink { get; set; }
    }
}
