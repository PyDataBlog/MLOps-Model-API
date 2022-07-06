using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Host
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }


        private ServiceContract.MeteoServiceContract service;
        private System.ServiceModel.ServiceHost host;

        private void Form1_Load(object sender, EventArgs e)
        {
            service = new Service.MeteoService();
            host = new System.ServiceModel.ServiceHost(
                    typeof(Service.MeteoService));
            host.Open();
        }

        private void Form1_FormClosing(object sender, FormClosingEventArgs e)
        {
            host.Close();
        }

        private void label1_Click(object sender, EventArgs e)
        {

        }
    }
}
