using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace UserInterface
{
    public partial class SubjectForm : Form
    {
        MainMenu parent;
        public SubjectForm(MainMenu parent)
        {
            InitializeComponent();
            this.parent = parent;
        }

        private void SubjectForm_Load(object sender, EventArgs e)
        {
            // TODO: This line of code loads data into the 'gradingsystemDataSet6.professor' table. You can move, or remove it, as needed.
            this.professorTableAdapter.Fill(this.gradingsystemDataSet6.professor);
            // TODO: This line of code loads data into the 'gradingsystemDataSet5.course' table. You can move, or remove it, as needed.
            this.courseTableAdapter.Fill(this.gradingsystemDataSet5.course);
            // TODO: This line of code loads data into the 'gradingsystemDataSet4.subject' table. You can move, or remove it, as needed.
            this.subjectTableAdapter.Fill(this.gradingsystemDataSet4.subject);

        }

        private void addSubject(object sender, EventArgs e)
        {

            try
            {
                if (txtSubjectCode.Text != "" && cboCourseCode.SelectedIndex != -1 && cboProfCode.SelectedIndex != -1)
                {
                    subjectTableAdapter.addSubject(txtSubjectCode.Text, cboCourseCode.SelectedIndex, cboProfCode.SelectedIndex);
                    MessageBox.Show("Successfully Added");
                    txtSubjectCode.Text = "";
                    cboCourseCode.SelectedIndex = -1;
                    cboProfCode.SelectedIndex = -1;
                }

                else
                {
                    MessageBox.Show("Please fill up all fields!");
                }

            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message);
            }

        }

        private void clearFields(object sender, EventArgs e)
        {

            txtSubjectCode.Text = "";
            cboCourseCode.SelectedIndex = -1;
            cboProfCode.SelectedIndex = -1;
        }

    }
}
