using System;
using System.Globalization;
using System.Xml.Serialization;

namespace MyFlightscope.Proxy.DTO
{
    /// <summary>
    /// 2016-03-03 07:24:05
    /// </summary>
    public class MyFlightDatetime : IXmlSerializable, IFormattable, IComparable<DateTime>, IEquatable<DateTime>
    {
        public const string DATEFORMAT = "yyyy-MM-dd HH:mm:ss";

        private DateTime dateTime;

        public System.Xml.Schema.XmlSchema GetSchema()
        {
            return null;
        }

        public void ReadXml(System.Xml.XmlReader reader)
        {
            string elementValue = reader.IsEmptyElement ? null : reader.ReadElementString();

            dateTime = DateTime.ParseExact(elementValue, DATEFORMAT, CultureInfo.InvariantCulture);
        }

        public void WriteXml(System.Xml.XmlWriter writer)
        {
            writer.WriteString(dateTime.ToString(DATEFORMAT, CultureInfo.InvariantCulture));
        }

        public DateTime Value { get { return dateTime; } }

        public override string ToString()
        {
            return Value.ToString();
        }

        public int CompareTo(object obj)
        {
            return Value.CompareTo(obj);
        }

        public string ToString(string format)
        {
            return Value.ToString(format);
        }

        public string ToString(string format, IFormatProvider formatProvider)
        {
            return Value.ToString(format, formatProvider);
        }

        public int CompareTo(DateTime other)
        {
            return Value.CompareTo(other);
        }

        public bool Equals(DateTime other)
        {
            return Value.Equals(other);
        }
    }
}