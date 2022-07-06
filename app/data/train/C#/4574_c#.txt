using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace CalcDatabaseSize
{
    /// <summary>
    /// Contains the main code for estimator.
    /// </summary>
    public partial class MainForm : Form
    {
        public SqlConnection SqlConn;
        private string _file;

        public MainForm()
        {
            InitializeComponent();

            tbServerName.Text = Properties.Settings.Default.ServerName;
        }

        private void tbServerName_Event(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                btnConnect_Click(sender, e);
            }
        }

        private void btnConnect_Click(object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            ckbAllTables.Checked = false;
            ChangeView();

            Properties.Settings.Default["ServerName"] = tbServerName.Text;
            Properties.Settings.Default.Save();
            try
            {
                // Connect to the Database
                using (SqlConn = new SqlConnection("Data Source=" + tbServerName.Text + ";Integrated Security=SSPI;"))
                {
                    SqlConn.Open();
                    DataTable tblDatabases = SqlConn.GetSchema("Databases");
                    SqlConn.Close();

                    List<string> databases = new List<string>();
                    foreach (DataRow row in tblDatabases.Rows)
                    {
                        string strDatabaseName = row["database_name"].ToString();

                        databases.Add(strDatabaseName);
                    }
                    cbDatabases.DataSource = databases;
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(null, ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            Cursor.Current = Cursors.Default;
        }

        private void cbDatabases_SelectedIndexChanged(object sender, EventArgs e)
        {
            try
            {
                // Connect to the database and get all the tables
                using (SqlConn = new SqlConnection("Data Source=" + tbServerName.Text + ";Initial Catalog=" + cbDatabases.Text + ";Integrated Security=SSPI;"))
                {
                    SqlConn.Open();

                    List<string> tables = new List<string>();
                    DataTable schema = SqlConn.GetSchema("Tables");
                    foreach (DataRow row in schema.Rows)
                    {
                        tables.Add(row[1].ToString() + "." + row[2].ToString());
                    }
                    cbTables.DataSource = tables;
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(null, ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            ChangeView();
        }

        private void btnCalculate_Click(object sender, EventArgs e)
        {
            StringBuilder sb = new StringBuilder();
            decimal totalMb = 0;

            // Loop through all the text controls on the MultiTable panel
            if (ckbAllTables.Checked)
            {
                foreach (TableControle ctrl in pnlMultiTable.Controls)
                {
                    CalculateTable(ref sb, ref totalMb, ctrl.TableName, ctrl.Rows);
                }
            }
            else
            {
                int rows;
                int.TryParse(tbRows.Text, out rows);
                CalculateTable(ref sb, ref totalMb, cbTables.Text, rows);
            }

            lblTotalSize.Text = totalMb.ToString("0.##") + " MB";
            _file = sb.ToString();
        }

        private long GetMaximumSize(string column)
        {
            SqlConn = new SqlConnection("Data Source=" + tbServerName.Text + ";Initial Catalog=" + cbDatabases.Text + ";Integrated Security=SSPI;");

            using (SqlCommand command = SqlConn.CreateCommand())
            {
                command.CommandText = "SELECT max(len([" + column + "])) as [MaxSize] FROM " + cbDatabases.Text + "." + cbTables.Text;
                SqlConn.Open();

                using (SqlDataReader reader = command.ExecuteReader())
                {
                    reader.Read();
                    int result;
                    int.TryParse(reader["maxSize"].ToString(), out result);

                    return result;
                }
            }
        }

        private void btnReport_Click(object sender, EventArgs e)
        {
            try
            {
                SaveFileDialog saveReport = new SaveFileDialog
                {
                    Filter = "txt files (*.txt)|*.txt|All files (*.*)|*.*",
                    FilterIndex = 2,
                    RestoreDirectory = true,
                    DefaultExt = "txt",
                    FileName = cbDatabases.Text + "_" + cbTables.Text + ".txt"
                };

                if (saveReport.ShowDialog() == DialogResult.OK)
                {
                    System.IO.File.WriteAllText(saveReport.FileName, _file);
                    System.Diagnostics.Process.Start(saveReport.FileName);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(null, ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ckbAllTables_CheckedChanged(object sender, EventArgs e)
        {
            ChangeView();
        }

        public void ChangeView()
        {
            pnlMultiTable.Controls.Clear();
            pnlBottom.Location = new Point(10, 150);

            if (ckbAllTables.Checked)
            {
                pnlSingleTable.Visible = false;
                pnlMultiTable.Visible = true;
            }
            else
            {
                pnlSingleTable.Visible = true;
                pnlMultiTable.Visible = false;
            }

            if (pnlMultiTable.Visible)
            {
                pnlBottom.Location = new Point(10, 80);
                for (int i = 0; i < cbTables.Items.Count; i++)
                {
                    TableControle tControle = new TableControle();
                    tControle.Location = new Point(3, 5 + i * 30);
                    tControle.TableName = cbTables.Items[i].ToString();
                    pnlMultiTable.Controls.Add(tControle);

                    if (pnlBottom.Location.Y > 650) continue;
                    pnlBottom.Location = new Point(pnlBottom.Location.X, pnlBottom.Location.Y + 30);
                }
            }
        }

        private void pnlMultiTable_MouseEnter(object sender, EventArgs e)
        {
            pnlMultiTable.Focus();
        }

        public void CalculateTable(ref StringBuilder sb, ref decimal totalMb, string tableName, int rows)
        {
            try
            {
                SqlConn = new SqlConnection("Data Source=" + tbServerName.Text + ";Initial Catalog=" + cbDatabases.Text + ";Integrated Security=SSPI;");

                using (SqlCommand command = SqlConn.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM " + tableName;
                    SqlConn.Open();

                    long totalBytes = 0;

                    using (SqlDataReader reader = command.ExecuteReader(CommandBehavior.SchemaOnly))
                    {
                        reader.Read();
                        DataTable schema = reader.GetSchemaTable();

                        List<TableRow> tableRows = new List<TableRow>();
                        int nameMaxLength = 4;
                        int sizeLength = 5;
                        if (schema != null)
                            foreach (DataRow row in schema.Rows)
                            {
                                long size = long.Parse(row["ColumnSize"].ToString());
                                string type = row["DataTypeName"].ToString();
                                string name = row["ColumnName"].ToString();

                                // condition ? true : false
                                long bytes = size > 2000000000 ? (long)(GetMaximumSize(name) * 0.15) : size;

                                TableRow tableRow = new TableRow
                                {
                                    Name = name,
                                    Size = bytes,
                                    DataType = type
                                };

                                if (nameMaxLength < tableRow.Name.Length) nameMaxLength = tableRow.Name.Length;

                                if (sizeLength < tableRow.Size.ToString().Length) sizeLength = tableRow.Size.ToString().Length;

                                tableRows.Add(tableRow);
                            }

                        // Add one to the max length, for the correct format.
                        nameMaxLength++;
                        sizeLength++;
                        string format = "{0,-" + nameMaxLength + "}{1,-" + sizeLength + "}{2,-10}";
                        sb.AppendFormat(format, "Name", "Bytes", "Type").AppendLine().AppendLine();

                        foreach (TableRow row in tableRows)
                        {
                            sb.AppendFormat(format, row.Name, row.Size, row.DataType).AppendLine();
                            totalBytes += row.Size;
                        }
                    }
                    SqlConn.Close();
                    
                    sb.AppendLine();
                    sb.AppendFormat("{0,-25}{1,-5}", "Bytes per Row", totalBytes).AppendLine();
                    sb.AppendFormat("{0,-25}{1,-5}", "Rows", rows).AppendLine();
                    sb.AppendFormat("{0,-25}{1,-5}", "Compression", nudCompression.Value + "%").AppendLine();
                    sb.AppendFormat("{0,-25}{1,-5}", "Index Ratio", nudIndexRatio.Value + "%").AppendLine();
                    sb.AppendLine();

                    decimal totalSize = totalBytes * rows;
                    totalSize *= (100 + nudIndexRatio.Value) / 100;
                    totalSize *= (100 - nudCompression.Value) / 100;
                    
                    totalMb = totalSize / 1048576;
                    decimal totalGb = totalSize / 1073741824;
                    sb.AppendLine(totalSize + " bytes");
                    
                    sb.AppendLine(totalMb.ToString("0.##") + " MB");
                    sb.AppendLine(totalGb.ToString("0.##") + " GB");

                    sb.AppendLine().AppendLine();
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(null, ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}
