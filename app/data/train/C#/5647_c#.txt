using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace WolframAlpha
{
    [XmlRoot("queryresult")]
    public class QueryResult
    {
        [XmlAttribute("success")]
        public bool Success { get; set; }

        [XmlAttribute("error")]
        public bool Error { get; set; }

        [XmlAttribute("datatypes")]
        public string DataTypesString { get; set; }

        [XmlIgnore]
        public string[] DataTypes // change to IEnumerable<string> for .net 4.5
        {
            get
            {
                return string.IsNullOrEmpty(DataTypesString) ? new string[] { } : DataTypesString.Split(',');
            }
            set
            {
                DataTypesString = String.Join(",", value);
            }
        }

        [XmlAttribute("timedout")]
        public string TimedOutString { get; set; }

        [XmlIgnore]
        public string[] TimedOut // change to IEnumerable<string> for .net 4.5
        {
            get
            {
                return string.IsNullOrEmpty(TimedOutString)? new string[] {} : TimedOutString.Split(',');
            }
            set
            {
                TimedOutString = String.Join(",", value);
            }
        }

        [XmlAttribute("timing")]
        public double Timing { get; set; }
        
        [XmlAttribute("parsetiming")]
        public double ParseTiming { get; set; }

        [XmlAttribute("parsetimedout")]
        public bool ParseTimedOut { get; set; }

        [XmlElement("pod")]
        public List<Pod> Pods { get; set; }

        [XmlArray("assumptions"), XmlArrayItem("assumption")]
        public List<Assumption> Assumptions { get; set; }
        
        [XmlArray("sources"), XmlArrayItem("source")]
        public List<Source> Sources { get; set; }

        [XmlElement("warnings")]
        public Warnings Warnings { get; set; }

        [XmlElement("generalization")]
        public List<Generalization> Generalizations { get; set; } 
    }
}
