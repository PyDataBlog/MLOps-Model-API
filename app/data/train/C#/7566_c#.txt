namespace Datalogger
{
    partial class signup
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(signup));
            this.up_panel = new System.Windows.Forms.Panel();
            this.pictureBox3 = new System.Windows.Forms.PictureBox();
            this.title = new System.Windows.Forms.Button();
            this.exit = new System.Windows.Forms.Button();
            this.downpanel = new System.Windows.Forms.Panel();
            this.firstname = new System.Windows.Forms.TextBox();
            this.password = new System.Windows.Forms.TextBox();
            this.username = new System.Windows.Forms.TextBox();
            this.confirm_password = new System.Windows.Forms.TextBox();
            this.fname = new System.Windows.Forms.Label();
            this.lname = new System.Windows.Forms.Label();
            this.Gender = new System.Windows.Forms.Label();
            this.lastname = new System.Windows.Forms.TextBox();
            this.M = new System.Windows.Forms.RadioButton();
            this.F = new System.Windows.Forms.RadioButton();
            this.uname = new System.Windows.Forms.Label();
            this.pwd = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.mail = new System.Windows.Forms.Label();
            this.mailid = new System.Windows.Forms.TextBox();
            this.dob = new System.Windows.Forms.DateTimePicker();
            this.label1 = new System.Windows.Forms.Label();
            this.signup_database = new System.Windows.Forms.Button();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.reqlev = new System.Windows.Forms.Label();
            this.up_panel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox3)).BeginInit();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // up_panel
            // 
            this.up_panel.BackColor = System.Drawing.SystemColors.GrayText;
            this.up_panel.Controls.Add(this.pictureBox3);
            this.up_panel.Controls.Add(this.title);
            this.up_panel.Controls.Add(this.exit);
            this.up_panel.Dock = System.Windows.Forms.DockStyle.Top;
            this.up_panel.Location = new System.Drawing.Point(0, 0);
            this.up_panel.Name = "up_panel";
            this.up_panel.Size = new System.Drawing.Size(548, 29);
            this.up_panel.TabIndex = 9;
            this.up_panel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.panelmain_MouseDown);
            this.up_panel.MouseMove += new System.Windows.Forms.MouseEventHandler(this.panelmain_MouseMove);
            this.up_panel.MouseUp += new System.Windows.Forms.MouseEventHandler(this.panelmain_MouseUp);
            // 
            // pictureBox3
            // 
            this.pictureBox3.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox3.Image")));
            this.pictureBox3.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.pictureBox3.Location = new System.Drawing.Point(2, 1);
            this.pictureBox3.Name = "pictureBox3";
            this.pictureBox3.Size = new System.Drawing.Size(29, 27);
            this.pictureBox3.SizeMode = System.Windows.Forms.PictureBoxSizeMode.CenterImage;
            this.pictureBox3.TabIndex = 13;
            this.pictureBox3.TabStop = false;
            // 
            // title
            // 
            this.title.FlatAppearance.BorderSize = 0;
            this.title.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.title.Font = new System.Drawing.Font("Tempus Sans ITC", 12F, System.Drawing.FontStyle.Bold);
            this.title.ForeColor = System.Drawing.SystemColors.ButtonHighlight;
            this.title.ImageAlign = System.Drawing.ContentAlignment.TopLeft;
            this.title.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.title.Location = new System.Drawing.Point(27, 0);
            this.title.Name = "title";
            this.title.Size = new System.Drawing.Size(116, 29);
            this.title.TabIndex = 5;
            this.title.Text = "SIGN UP";
            this.title.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.title.TextImageRelation = System.Windows.Forms.TextImageRelation.TextAboveImage;
            this.title.UseVisualStyleBackColor = false;
            // 
            // exit
            // 
            this.exit.BackColor = System.Drawing.Color.Gray;
            this.exit.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None;
            this.exit.ForeColor = System.Drawing.SystemColors.ControlText;
            this.exit.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.exit.Location = new System.Drawing.Point(517, 0);
            this.exit.Name = "exit";
            this.exit.Size = new System.Drawing.Size(30, 30);
            this.exit.TabIndex = 7;
            this.exit.Text = "X";
            this.exit.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.exit.UseVisualStyleBackColor = false;
            this.exit.Click += new System.EventHandler(this.exit_Click);
            this.exit.MouseLeave += new System.EventHandler(this.exit_MouseLeave);
            this.exit.MouseHover += new System.EventHandler(this.exit_MouseHover);
            // 
            // downpanel
            // 
            this.downpanel.BackColor = System.Drawing.SystemColors.GrayText;
            this.downpanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.downpanel.Location = new System.Drawing.Point(0, 311);
            this.downpanel.Name = "downpanel";
            this.downpanel.Size = new System.Drawing.Size(548, 29);
            this.downpanel.TabIndex = 10;
            // 
            // firstname
            // 
            this.firstname.Location = new System.Drawing.Point(127, 23);
            this.firstname.Name = "firstname";
            this.firstname.Size = new System.Drawing.Size(178, 20);
            this.firstname.TabIndex = 12;
            // 
            // password
            // 
            this.password.Location = new System.Drawing.Point(127, 108);
            this.password.Name = "password";
            this.password.Size = new System.Drawing.Size(178, 20);
            this.password.TabIndex = 13;
            // 
            // username
            // 
            this.username.Location = new System.Drawing.Point(127, 79);
            this.username.Name = "username";
            this.username.Size = new System.Drawing.Size(178, 20);
            this.username.TabIndex = 15;
            // 
            // confirm_password
            // 
            this.confirm_password.Location = new System.Drawing.Point(127, 138);
            this.confirm_password.Name = "confirm_password";
            this.confirm_password.Size = new System.Drawing.Size(178, 20);
            this.confirm_password.TabIndex = 16;
            // 
            // fname
            // 
            this.fname.AutoSize = true;
            this.fname.Location = new System.Drawing.Point(44, 23);
            this.fname.Name = "fname";
            this.fname.Size = new System.Drawing.Size(60, 13);
            this.fname.TabIndex = 17;
            this.fname.Text = "First Name:";
            // 
            // lname
            // 
            this.lname.AutoSize = true;
            this.lname.Location = new System.Drawing.Point(311, 23);
            this.lname.Name = "lname";
            this.lname.Size = new System.Drawing.Size(61, 13);
            this.lname.TabIndex = 18;
            this.lname.Text = "Last Name:";
            // 
            // Gender
            // 
            this.Gender.AutoSize = true;
            this.Gender.Location = new System.Drawing.Point(59, 56);
            this.Gender.Name = "Gender";
            this.Gender.Size = new System.Drawing.Size(45, 13);
            this.Gender.TabIndex = 19;
            this.Gender.Text = "Gender:";
            // 
            // lastname
            // 
            this.lastname.Location = new System.Drawing.Point(373, 23);
            this.lastname.Name = "lastname";
            this.lastname.Size = new System.Drawing.Size(146, 20);
            this.lastname.TabIndex = 20;
            this.lastname.TextChanged += new System.EventHandler(this.lastname_TextChanged);
            // 
            // M
            // 
            this.M.AutoSize = true;
            this.M.Location = new System.Drawing.Point(127, 56);
            this.M.Name = "M";
            this.M.Size = new System.Drawing.Size(47, 17);
            this.M.TabIndex = 21;
            this.M.TabStop = true;
            this.M.Text = "male";
            this.M.UseVisualStyleBackColor = true;
            this.M.CheckedChanged += new System.EventHandler(this.M_CheckedChanged);
            // 
            // F
            // 
            this.F.AutoSize = true;
            this.F.Location = new System.Drawing.Point(180, 56);
            this.F.Name = "F";
            this.F.Size = new System.Drawing.Size(56, 17);
            this.F.TabIndex = 22;
            this.F.TabStop = true;
            this.F.Text = "female";
            this.F.UseVisualStyleBackColor = true;
            this.F.CheckedChanged += new System.EventHandler(this.F_CheckedChanged);
            // 
            // uname
            // 
            this.uname.AutoSize = true;
            this.uname.ForeColor = System.Drawing.Color.DarkRed;
            this.uname.Location = new System.Drawing.Point(46, 79);
            this.uname.Name = "uname";
            this.uname.Size = new System.Drawing.Size(62, 13);
            this.uname.TabIndex = 23;
            this.uname.Text = "Username:*";
            // 
            // pwd
            // 
            this.pwd.AutoSize = true;
            this.pwd.ForeColor = System.Drawing.Color.DarkRed;
            this.pwd.Location = new System.Drawing.Point(48, 108);
            this.pwd.Name = "pwd";
            this.pwd.Size = new System.Drawing.Size(60, 13);
            this.pwd.TabIndex = 24;
            this.pwd.Text = "Password:*";
            this.pwd.Click += new System.EventHandler(this.pwd_Click);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.ForeColor = System.Drawing.Color.DarkRed;
            this.label3.Location = new System.Drawing.Point(10, 138);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(98, 13);
            this.label3.TabIndex = 25;
            this.label3.Text = "Confirm Password:*";
            // 
            // mail
            // 
            this.mail.AutoSize = true;
            this.mail.ForeColor = System.Drawing.Color.DarkRed;
            this.mail.Location = new System.Drawing.Point(69, 173);
            this.mail.Name = "mail";
            this.mail.Size = new System.Drawing.Size(39, 13);
            this.mail.TabIndex = 27;
            this.mail.Text = "Email:*";
            // 
            // mailid
            // 
            this.mailid.Location = new System.Drawing.Point(127, 168);
            this.mailid.Name = "mailid";
            this.mailid.Size = new System.Drawing.Size(178, 20);
            this.mailid.TabIndex = 26;
            // 
            // dob
            // 
            this.dob.Location = new System.Drawing.Point(127, 199);
            this.dob.Name = "dob";
            this.dob.Size = new System.Drawing.Size(178, 20);
            this.dob.TabIndex = 29;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(71, 199);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(33, 13);
            this.label1.TabIndex = 30;
            this.label1.Text = "DOB:";
            // 
            // signup_database
            // 
            this.signup_database.Location = new System.Drawing.Point(465, 282);
            this.signup_database.Name = "signup_database";
            this.signup_database.Size = new System.Drawing.Size(75, 23);
            this.signup_database.TabIndex = 31;
            this.signup_database.Text = "Sign Me UP";
            this.signup_database.UseVisualStyleBackColor = true;
            this.signup_database.Click += new System.EventHandler(this.signup_database_Click);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.password);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.firstname);
            this.groupBox1.Controls.Add(this.dob);
            this.groupBox1.Controls.Add(this.username);
            this.groupBox1.Controls.Add(this.mail);
            this.groupBox1.Controls.Add(this.confirm_password);
            this.groupBox1.Controls.Add(this.mailid);
            this.groupBox1.Controls.Add(this.fname);
            this.groupBox1.Controls.Add(this.label3);
            this.groupBox1.Controls.Add(this.lname);
            this.groupBox1.Controls.Add(this.pwd);
            this.groupBox1.Controls.Add(this.Gender);
            this.groupBox1.Controls.Add(this.uname);
            this.groupBox1.Controls.Add(this.lastname);
            this.groupBox1.Controls.Add(this.F);
            this.groupBox1.Controls.Add(this.M);
            this.groupBox1.Location = new System.Drawing.Point(12, 36);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(528, 235);
            this.groupBox1.TabIndex = 32;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "--------Sign Up------------------------------------------------------------------" +
    "--------------------------------------------------------------------------------" +
    "-";
            // 
            // reqlev
            // 
            this.reqlev.AutoSize = true;
            this.reqlev.Font = new System.Drawing.Font("Bradley Hand ITC", 11.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.reqlev.Location = new System.Drawing.Point(9, 282);
            this.reqlev.Name = "reqlev";
            this.reqlev.Size = new System.Drawing.Size(198, 19);
            this.reqlev.TabIndex = 33;
            this.reqlev.Text = "Note: * are required fields";
            // 
            // signup
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(548, 340);
            this.Controls.Add(this.reqlev);
            this.Controls.Add(this.signup_database);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.downpanel);
            this.Controls.Add(this.up_panel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
            this.Name = "signup";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "signup";
            this.up_panel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox3)).EndInit();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Panel up_panel;
        private System.Windows.Forms.Button title;
        private System.Windows.Forms.Button exit;
        private System.Windows.Forms.Panel downpanel;
        private System.Windows.Forms.TextBox firstname;
        private System.Windows.Forms.TextBox password;
        private System.Windows.Forms.TextBox username;
        private System.Windows.Forms.TextBox confirm_password;
        private System.Windows.Forms.Label fname;
        private System.Windows.Forms.Label lname;
        private System.Windows.Forms.Label Gender;
        private System.Windows.Forms.TextBox lastname;
        private System.Windows.Forms.RadioButton M;
        private System.Windows.Forms.RadioButton F;
        private System.Windows.Forms.Label uname;
        private System.Windows.Forms.Label pwd;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label mail;
        private System.Windows.Forms.TextBox mailid;
        private System.Windows.Forms.DateTimePicker dob;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button signup_database;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.PictureBox pictureBox3;
        private System.Windows.Forms.Label reqlev;

    }
}