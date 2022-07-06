namespace EmployeeReview.Forms
{
    partial class SummaryForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.lbl_Summary = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // lbl_Summary
            // 
            this.lbl_Summary.AutoSize = true;
            this.lbl_Summary.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lbl_Summary.Location = new System.Drawing.Point(0, 0);
            this.lbl_Summary.Name = "lbl_Summary";
            this.lbl_Summary.Size = new System.Drawing.Size(35, 13);
            this.lbl_Summary.TabIndex = 0;
            this.lbl_Summary.Text = "label1";
            this.lbl_Summary.Click += new System.EventHandler(this.lbl_Summary_Click);
            // 
            // SummaryForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(737, 395);
            this.Controls.Add(this.lbl_Summary);
            this.Name = "SummaryForm";
            this.Text = "SummaryForm";
            this.Load += new System.EventHandler(this.SummaryForm_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label lbl_Summary;
    }
}