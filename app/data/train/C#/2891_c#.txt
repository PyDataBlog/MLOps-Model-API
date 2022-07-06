using System;
using System.Globalization;
using Newtonsoft.Json;

using HolisticWare.Ph4ct3x.Sports.Judo.InternationalJudoFederation.Internal;

namespace HolisticWare.Ph4ct3x.Sports.Judo.InternationalJudoFederation.Internal
{
    public class SearchAllResult
    {
        [JsonProperty("module")]
        public Module Module { get; set; }

        [JsonProperty("label")]
        public string Label { get; set; }

        [JsonProperty("value")]
        public string Value { get; set; }

        [JsonProperty("id")]
        [JsonConverter(typeof(ParseStringConverter))]
        public long Id { get; set; }

        public static SearchAllResult[] FromJson(string json)
            =>
            JsonConvert.DeserializeObject<SearchAllResult[]>(json, Converter.Settings);
    }

    public enum Module
    {
        Competition,
        Competitor,
        Country,
        Weight,
        Tags
    }


    internal class ParseStringConverter : JsonConverter
    {
        public override bool CanConvert(Type t) => t == typeof(long) || t == typeof(long?);

        public override object ReadJson(JsonReader reader, Type t, object existingValue, JsonSerializer serializer)
        {
            if (reader.TokenType == JsonToken.Null) return null;
            var value = serializer.Deserialize<string>(reader);
            long l;
            if (Int64.TryParse(value, out l))
            {
                return l;
            }
            throw new Exception("Cannot unmarshal type long");
        }

        public override void WriteJson(JsonWriter writer, object untypedValue, JsonSerializer serializer)
        {
            if (untypedValue == null)
            {
                serializer.Serialize(writer, null);
                return;
            }
            var value = (long)untypedValue;
            serializer.Serialize(writer, value.ToString());
            return;
        }

        public static readonly ParseStringConverter Singleton = new ParseStringConverter();
    }

    internal class ModuleConverter : JsonConverter
    {
        public override bool CanConvert(Type t) => t == typeof(Module) || t == typeof(Module?);

        public override object ReadJson(JsonReader reader, Type t, object existingValue, JsonSerializer serializer)
        {
            if (reader.TokenType == JsonToken.Null) return null;
            var value = serializer.Deserialize<string>(reader);
            switch (value)
            {
                case "competition":
                    return Module.Competition;
                case "competitor":
                    return Module.Competitor;
                case "country":
                    return Module.Country;
                case "weight":
                    return Module.Weight;
               case "tags":
                    return Module.Tags;
             }
            throw new Exception("Cannot unmarshal type Module");
        }

        public override void WriteJson(JsonWriter writer, object untypedValue, JsonSerializer serializer)
        {
            if (untypedValue == null)
            {
                serializer.Serialize(writer, null);
                return;
            }
            var value = (Module)untypedValue;
            switch (value)
            {
                case Module.Competition:
                    serializer.Serialize(writer, "competition");
                    return;
                case Module.Competitor:
                    serializer.Serialize(writer, "competitor");
                    return;
                case Module.Country:
                    serializer.Serialize(writer, "country");
                    return;
                case Module.Tags:
                    serializer.Serialize(writer, "tags");
                    return;
                case Module.Weight:
                    serializer.Serialize(writer, "weight");
                    return;
            }
            throw new Exception("Cannot marshal type Module");
        }

        public static readonly ModuleConverter Singleton = new ModuleConverter();
    }
}




