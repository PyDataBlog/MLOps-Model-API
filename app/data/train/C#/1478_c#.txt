using Newtonsoft.Json.Linq;
using System.Collections.Generic;   

namespace BlizzardAPI.Wow.DataResources
{
    public class CharacterRaces
    {
        private List<CharacterRace> races = new List<CharacterRace>();
        public List<CharacterRace> Races { get => races; set => races = value; }
        public CharacterRaces(string region, string locale)
        {
            var racesData = ApiHelper.GetJsonFromUrl(
              $"https://{region}.api.battle.net/wow/data/character/races?locale={locale}&apikey={ApiHandler.ApiKey}"
          );

            if (racesData == null)
                return;

            for (var i = 0; i < (racesData["races"] as JArray).Count; i++)
            {
                Races.Add(new CharacterRace
                {
                    Id = racesData["races"][i]["id"],
                    Mask = racesData["races"][i]["mask"],
                    Side = racesData["races"][i]["side"],
                    Name = racesData["races"][i]["name"]
                });
            }
        }
    }
}
