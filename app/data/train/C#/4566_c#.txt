// Type: Highsoft.Web.Mvc.Stocks.AreasplinerangeSeriesZones


using System.Collections;
using Newtonsoft.Json;

namespace Highsoft.Web.Mvc.Stocks
{
    public class AreasplinerangeSeriesZones : BaseObject
    {
        public string ClassName { get; set; }

        private string ClassName_DefaultValue { get; set; }

        public string Color { get; set; }

        private string Color_DefaultValue { get; set; }

        public string DashStyle { get; set; }

        private string DashStyle_DefaultValue { get; set; }

        public object FillColor { get; set; }

        private object FillColor_DefaultValue { get; set; }

        public double? Value { get; set; }

        private double? Value_DefaultValue { get; set; }

        public AreasplinerangeSeriesZones()
        {
            this.ClassName = this.ClassName_DefaultValue = "";
            this.Color = this.Color_DefaultValue = (string) null;
            this.DashStyle = this.DashStyle_DefaultValue = (string) null;
            this.FillColor = this.FillColor_DefaultValue = (object) null;
            double? nullable = new double?();
            this.Value_DefaultValue = nullable;
            this.Value = nullable;
        }

        internal override Hashtable ToHashtable()
        {
            Hashtable hashtable = new Hashtable();
            if (this.ClassName != this.ClassName_DefaultValue)
                hashtable.Add((object) "className", (object) this.ClassName);
            if (this.Color != this.Color_DefaultValue)
                hashtable.Add((object) "color", (object) this.Color);
            if (this.DashStyle != this.DashStyle_DefaultValue)
                hashtable.Add((object) "dashStyle", (object) this.DashStyle);
            if (this.FillColor != this.FillColor_DefaultValue)
                hashtable.Add((object) "fillColor", this.FillColor);
            double? nullable = this.Value;
            double? valueDefaultValue = this.Value_DefaultValue;
            if (nullable.GetValueOrDefault() != valueDefaultValue.GetValueOrDefault() ||
                nullable.HasValue != valueDefaultValue.HasValue)
                hashtable.Add((object) "value", (object) this.Value);
            return hashtable;
        }

        internal override string ToJSON()
        {
            Hashtable hashtable = this.ToHashtable();


            if (hashtable.Count > 0)
                return JsonConvert.SerializeObject((object) this.ToHashtable());
            return "";
        }

        internal override bool IsDirty()
        {
            return this.ToHashtable().Count > 0;
        }
    }
}