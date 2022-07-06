using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace SoccerManager.UI.Base
{
    public class BaseForm : Form
    {
        public BaseForm()
        {
            KeyPreview = true;
            KeyDown += BaseForm_KeyDown;
        }

        private void BaseForm_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                SelectNextControl(ActiveControl, !e.Shift, true, true, true);

                e.Handled = true;
                e.SuppressKeyPress = true;
            }
            else if (e.KeyCode == Keys.Escape)
            {
                if (Application.OpenForms.Count > 1)
                    Close();
                else
                    Application.Exit();

                e.Handled = true;
                e.SuppressKeyPress = true;
            }
        }

        public BaseForm(Form form) : this()
        {
            MdiParent = form.MdiParent ?? form;
        }

        protected override void OnFormClosing(FormClosingEventArgs e)
        {
            if (Application.OpenForms.Count == 1)
            {
                var result = MessageBox.Show("Deseja realmente sair?", "Atenção", MessageBoxButtons.YesNo, MessageBoxIcon.Question);

                if (result == DialogResult.Yes)
                    base.OnClosing(e);
                else
                    e.Cancel = true;
            }
        }
    }
}
