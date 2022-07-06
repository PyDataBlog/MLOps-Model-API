using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Collections;
using System.Threading;


namespace Joddgewe
{
    public partial class Form1 : Form
    {
        private string filenameOrto;
        
        public Form1()
        {
            InitializeComponent();
            toolStripStatusLabel1.Text = "Venter på at brukeren skal åpne ortofoto...";
        }


        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            addFiles();
        }

        private void closeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            closeApp();
        }


        public void closeApp()
        {
            this.Dispose();
        }

        public void addFiles()
        {
            Stream myStream;
            OpenFileDialog openFileDialog1 = new OpenFileDialog();
            
            openFileDialog1.Filter = "Ortofoto(*.tif;*.jpg;*.gif)|*.TIF;*.JPG;*.GIF";
            openFileDialog1.FilterIndex = 3;
            openFileDialog1.RestoreDirectory = true;
            openFileDialog1.Multiselect = true;
            
            if (openFileDialog1.ShowDialog(this) == DialogResult.OK)
            {

                if ((myStream = openFileDialog1.OpenFile()) != null)
                {
                    string[] files = openFileDialog1.FileNames;

                    foreach (string file in files )
                    {
                        Image bm = Image.FromFile(file);
                        FileHandler fh = new FileHandler(file, bm.Width, bm.Height);
                        
                        bm.Dispose();
                        listBoxFiler.Items.Add(fh);                      
                    }

                    myStream.Close();
                    enableButtons();
                    toolStripStatusLabel1.Text = "Venter på at brukeren skal velge utformat...";
                    
                }
                myStream.Dispose();
                lblVisEgenskaper.Visible = true;
                lblUtformat.Visible = true;
            }

            openFileDialog1.Dispose();
        }

        private void removeFiles()
        {
            listBoxFiler.Items.Remove(listBoxFiler.SelectedItem);
            textBoxMMM.Text = "";
            textBoxJGW.Text = "";
            textBoxSOSI.Text = "";
        }


        private void btnKonverter_Click(object sender, EventArgs e)
        {

        }

        private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Form2 frm = new Form2(this);
            frm.ShowDialog();
        }

        private void btnAdd_Click(object sender, EventArgs e)
        {
            addFiles();
        }

        private void btnRemove_Click(object sender, EventArgs e)
        {
            removeFiles();
        }

        private void listBoxFiler_SelectedIndexChanged(object sender, EventArgs e)
        {
            pictboxOrtofoto.Image.Dispose();

            if (listBoxFiler.SelectedItems.Count == 0)
            {
                System.ComponentModel.ComponentResourceManager resources = 
                    new System.ComponentModel.ComponentResourceManager(typeof(Form1));
                pictboxOrtofoto.Image = 
                    ((System.Drawing.Image)(resources.GetObject("pictboxOrtofoto.Image")));
            }
            else
            {
                FileHandler fh = (FileHandler)listBoxFiler.SelectedItem;
                filenameOrto = fh.ToString();
                try
                {
                    pictboxOrtofoto.Image = (Bitmap)Image.FromFile(filenameOrto);
                    displayStyringsfiler();
                }
                catch
                {
                    System.ComponentModel.ComponentResourceManager resources =
                         new System.ComponentModel.ComponentResourceManager(typeof(Form1));
                    pictboxOrtofoto.Image = 
                        ((System.Drawing.Image)(resources.GetObject("pictboxOrtofoto.ErrorImage")));
                }
               
            }

            if (listBoxFiler.Items.Count > 0) enableButtons();
            else disableButtons();
        }

        private void disableButtons()
        {
            radioButtonJGW.Enabled = false;
            radioButtonMMM.Enabled = false;
            radioButtonSOSI.Enabled = false;
            checkBoxFilliste.Enabled = false;
            btnFullfør.Enabled = false;
        }

        private void enableButtons()
        {
            radioButtonJGW.Enabled = true;
            radioButtonMMM.Enabled = true;
            radioButtonSOSI.Enabled = true;
            checkBoxFilliste.Enabled = true;
            if (radioButtonJGW.Checked || radioButtonMMM.Checked || radioButtonSOSI.Checked) 
                btnFullfør.Enabled = true;
        }


        private void displayStyringsfiler()
        {
            try
            {
                FileHandler fh = (FileHandler) listBoxFiler.SelectedItem;

                textBoxJGW.Text = fh.toJGW();
                textBoxMMM.Text = fh.toMMM();
                textBoxSOSI.Text = fh.toSOSI();
            }
            catch 
            {               
                textBoxMMM.Text = "";
                textBoxJGW.Text = "";
                textBoxSOSI.Text = "";
            }
        }

        private void btnFullfør_Click(object sender, EventArgs e)
        {
            string fileList = "";
            FileHandler fh1 = null;

            foreach (FileHandler fh in listBoxFiler.Items)
            {
                if (checkBoxFilliste.Checked) fileList += fh.ToString() + "\r\n";
                if (radioButtonJGW.Checked) fh.writeToFile(FileHandler.TYPE_JGW);
                if (radioButtonMMM.Checked) fh.writeToFile(FileHandler.TYPE_MMM);
                if (radioButtonSOSI.Checked) fh.writeToFile(FileHandler.TYPE_SOSI);
            }

            try
            {
                fh1 = (FileHandler)listBoxFiler.Items[0];
            }
            catch { }

            if (fh1 != null && checkBoxFilliste.Checked)
            {
                fh1.createFileList(fileList);
            }

            toolStripStatusLabel1.Text = "Suksess!";
            MessageBox.Show("Ferdig med å konvertere!", "JoddGewe 0.1", 
                MessageBoxButtons.OK, MessageBoxIcon.Information);

        }

        private void radioButtonJGW_CheckedChanged(object sender, EventArgs e)
        {
            btnFullfør.Enabled = true;
            toolStripStatusLabel1.Text = "Klar til å konvertere styringsfiler!";
        }

        private void radioButtonMMM_CheckedChanged(object sender, EventArgs e)
        {
            btnFullfør.Enabled = true;
            toolStripStatusLabel1.Text = "Klar til å konvertere styringsfiler!";
        }

        private void radioButtonSOSI_CheckedChanged(object sender, EventArgs e)
        {
            btnFullfør.Enabled = true;
            toolStripStatusLabel1.Text = "Klar til å konvertere styringsfiler!";
        }


   }

}