/*
    BattleNetApi - A .NET battle.net API library.
    Copyright (C) 2016  Sebastian Grunow <sebastian@grunow-it.de>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

using System.Threading.Tasks;
using BattleNetApi.Client.Models.WoW;

// ReSharper disable once CheckNamespace
namespace BattleNetApi.Client
{
    public partial class WowApiClient
    {
        public static string GetBossUrl(int id)
        {
            return $"/wow/boss/{id}";
        }
        public static string GetBossListUrl()
        {
            return "/wow/boss/";
        }

        public Boss GetBoss(int id)
        {
            return GetApiResponse(ForgeApiRequest<Boss>(GetBossUrl(id)));
        }

        public async Task<Boss> GetBossAsync(int id)
        {
            return await GetApiResponseAsync(ForgeApiRequest<Boss>(GetBossUrl(id)));
        }

        public BossList GetBossList()
        {
            return GetApiResponse(ForgeApiRequest<BossList>(GetBossListUrl()));
        }

        public async Task<BossList> GetBossListAsync()
        {
            return await GetApiResponseAsync(ForgeApiRequest<BossList>(GetBossListUrl()));
        }
    }
}
