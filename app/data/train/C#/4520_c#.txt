using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace PI_VJ4
{
    public partial class RM_01 : Form
    {
        public RM_01()
        {
            InitializeComponent();
        }

        private void RM_01_Load(object sender, EventArgs e)
        {

        }

        private void btnIzracunaj_Click(object sender, EventArgs e)
        {
            //Deklaracija lokalnih varijabli
            float polumjer = 0, povrsina = 0, opseg = 0;

            //Dohvaćanje vrijednosti iz TextBox kontrola u varijablu,
            //parsiranje automatski onemogućava i pojavu nenumeričkih vrijednosti
            float.TryParse(txtPolumjer.Text, out polumjer);
            //Onemogućavanje vrijednosti manjih od nule
            if (polumjer < 0) polumjer = 0;

            //Izračunavanje površine i opsega kruga.
            povrsina = polumjer * polumjer * (float)Math.PI;
            opseg = 2 * polumjer * (float)Math.PI;

            //Pohrana vrijednosti iz varijabli natrag u TextBox kontrole.
            txtPolumjer.Text = polumjer.ToString();
            txtPovrsina.Text = Math.Round(povrsina, 2).ToString();
            txtOpseg.Text = Math.Round(opseg, 2).ToString();

            //Spremanje vrijednosti polumjera u ListBox kontrolu
            //ako je CheckBox uključen
            if (chkSpremi.Checked == true)
            {
                lstRezultati.Items.Add(polumjer);
            }

        }
    }
}
