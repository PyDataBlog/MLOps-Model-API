using Q42.HueApi;
using Rca.HueSandbox;
using System;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HueSandbox_Form
{
    public class Controller
    {
        #region Member
        HueWrapper m_Hue;

        #endregion Member

        #region Properties
        /// <summary>
        /// Konfiguration der aktuell verbundenen Bridge
        /// </summary>
        public Bridge BridgeDetails { get; set; }

        #endregion Properties

        #region Constructor
        /// <summary>
        /// Leerer Konstruktor für Controller
        /// </summary>
        public Controller()
        {
            m_Hue = new HueWrapper();
        }

        #endregion Constructor

        #region Services
        /// <summary>
        /// Sucht nach Hue Bridges im lokalen Netzwerk
        /// </summary>
        /// <returns>IP Adressen der gefunden Hue Bridges</returns>
        public async Task<string[]> SearchBridges()
        {
            return await m_Hue.ScanBridges();
        }

        /// <summary>
        /// Baut eine Verbindung zur angegebenen Hue Bridge auf
        /// </summary>
        /// <returns>Hue Bridge</returns>
        public async Task<Bridge> ConnectBridge(string ip)
        {
            BridgeDetails = await m_Hue.Initialize(ip);

            return BridgeDetails;
        }

        /// <summary>
        /// Ruft Details des angegebenen Hue-Endpoints ab
        /// </summary>
        /// <param name="id">ID des Endpoints</param>
        /// <returns>Details des Endpoints</returns>
        public Light GetDeviceDetails(string id)
        {
            return BridgeDetails.Lights.First(x => x.Id == id);
        }

        /// <summary>
        /// Ruft Einstellungen des angegebenen Hue-Endpoints ab
        /// </summary>
        /// <param name="id">ID des Endpoints</param>
        /// <returns>Einstellungen des Endpoints</returns>
        public State GetSettings(string id)
        {
            var light = BridgeDetails.Lights.First(x => x.Id == id);

            return light.State;
        }

        /// <summary>
        /// Setzen einer Farbe auf angegebenen Hue-Endpoint
        /// </summary>
        /// <param name="id">ID des Endpoints</param>
        /// <param name="red">Hex-Wert für Rotanteil im RGB-Modell</param>
        /// <param name="green">Hex-Wert für Grünanteil im RGB-Modell</param>
        /// <param name="blue">Hex-Wert für Blauanteil im RGB-Modell</param>
        public void SetColor(string id, int red, int green, int blue)
        {
            var modell = BridgeDetails.Lights.First(x => x.Id == id).ModelId;

            m_Hue.SetColor(id, red, green, blue, modell);
        }

        /// <summary>
        /// Setzen der Helligkeit auf angegebenen Hue-Endpoint
        /// </summary>
        /// <param name="id">ID des Endpoints</param>
        /// <param name="brigthness">Hex-Wert für Helligkeit</param>
        public void SetBrigthness(string id, int brigthness)
        {
            m_Hue.SetBrigthness(id, brigthness);
        }

        /// <summary>
        /// Schalten des angegebenen Hue-Endpoints
        /// </summary>
        /// <param name="id">ID des Endpoints</param>
        /// <param name="state">On/Off-State</param>
        public void SwitchLight(string id, bool state)
        {
            m_Hue.SwitchLight(id, state);

        }

        #endregion Services

        #region Internal services
        #endregion Internal services

        #region Events
        #endregion Events
    }
}
