namespace ProjectManagement
{
    partial class EntryForm
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
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.EntrytextBox = new System.Windows.Forms.TextBox();
            this.PasstextBox = new System.Windows.Forms.TextBox();
            this.Entrybutton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(116, 28);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(108, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Потребителско име";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(148, 111);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(45, 13);
            this.label2.TabIndex = 1;
            this.label2.Text = "Парола";
            // 
            // EntrytextBox
            // 
            this.EntrytextBox.Location = new System.Drawing.Point(85, 68);
            this.EntrytextBox.Name = "EntrytextBox";
            this.EntrytextBox.Size = new System.Drawing.Size(169, 20);
            this.EntrytextBox.TabIndex = 2;
            // 
            // PasstextBox
            // 
            this.PasstextBox.Location = new System.Drawing.Point(102, 146);
            this.PasstextBox.Name = "PasstextBox";
            this.PasstextBox.PasswordChar = '*';
            this.PasstextBox.Size = new System.Drawing.Size(135, 20);
            this.PasstextBox.TabIndex = 3;
            // 
            // Entrybutton
            // 
            this.Entrybutton.Location = new System.Drawing.Point(131, 204);
            this.Entrybutton.Name = "Entrybutton";
            this.Entrybutton.Size = new System.Drawing.Size(75, 23);
            this.Entrybutton.TabIndex = 4;
            this.Entrybutton.Text = "Вход";
            this.Entrybutton.UseVisualStyleBackColor = true;
            this.Entrybutton.Click += new System.EventHandler(this.Entrybutton_Click);
            // 
            // EntryForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(344, 261);
            this.Controls.Add(this.Entrybutton);
            this.Controls.Add(this.PasstextBox);
            this.Controls.Add(this.EntrytextBox);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Name = "EntryForm";
            this.Text = "Потребителски вход в системата";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox EntrytextBox;
        private System.Windows.Forms.TextBox PasstextBox;
        private System.Windows.Forms.Button Entrybutton;
    }
}