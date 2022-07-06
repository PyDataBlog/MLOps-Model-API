namespace TimeZones
{
	partial class FormMain
	{
		/// <summary>
		/// 必需的设计器变量。
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// 清理所有正在使用的资源。
		/// </summary>
		/// <param name="disposing">如果应释放托管资源，为 true；否则为 false。</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing && (components != null))
			{
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Windows 窗体设计器生成的代码

		/// <summary>
		/// 设计器支持所需的方法 - 不要
		/// 使用代码编辑器修改此方法的内容。
		/// </summary>
		private void InitializeComponent()
		{
			this.cbSrcTimeZone = new System.Windows.Forms.ComboBox();
			this.label1 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.label3 = new System.Windows.Forms.Label();
			this.dtpSrcTime = new System.Windows.Forms.DateTimePicker();
			this.cbDestTimeZone = new System.Windows.Forms.ComboBox();
			this.label4 = new System.Windows.Forms.Label();
			this.btnSrcTimeNow = new System.Windows.Forms.Button();
			this.lblResult = new System.Windows.Forms.Label();
			this.btnConv = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// cbSrcTimeZone
			// 
			this.cbSrcTimeZone.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbSrcTimeZone.FormattingEnabled = true;
			this.cbSrcTimeZone.Location = new System.Drawing.Point(71, 46);
			this.cbSrcTimeZone.Name = "cbSrcTimeZone";
			this.cbSrcTimeZone.Size = new System.Drawing.Size(454, 20);
			this.cbSrcTimeZone.TabIndex = 0;
			// 
			// label1
			// 
			this.label1.AutoSize = true;
			this.label1.Location = new System.Drawing.Point(12, 49);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(53, 12);
			this.label1.TabIndex = 1;
			this.label1.Text = "从时区：";
			// 
			// label2
			// 
			this.label2.AutoSize = true;
			this.label2.Location = new System.Drawing.Point(12, 73);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(53, 12);
			this.label2.TabIndex = 2;
			this.label2.Text = "到时区：";
			// 
			// label3
			// 
			this.label3.AutoSize = true;
			this.label3.Location = new System.Drawing.Point(12, 25);
			this.label3.Name = "label3";
			this.label3.Size = new System.Drawing.Size(53, 12);
			this.label3.TabIndex = 3;
			this.label3.Text = "从时刻：";
			// 
			// dtpSrcTime
			// 
			this.dtpSrcTime.Format = System.Windows.Forms.DateTimePickerFormat.Custom;
			this.dtpSrcTime.Location = new System.Drawing.Point(71, 19);
			this.dtpSrcTime.Name = "dtpSrcTime";
			this.dtpSrcTime.ShowUpDown = true;
			this.dtpSrcTime.Size = new System.Drawing.Size(200, 21);
			this.dtpSrcTime.TabIndex = 4;
			// 
			// cbDestTimeZone
			// 
			this.cbDestTimeZone.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbDestTimeZone.FormattingEnabled = true;
			this.cbDestTimeZone.Location = new System.Drawing.Point(71, 73);
			this.cbDestTimeZone.Name = "cbDestTimeZone";
			this.cbDestTimeZone.Size = new System.Drawing.Size(454, 20);
			this.cbDestTimeZone.TabIndex = 5;
			// 
			// label4
			// 
			this.label4.AutoSize = true;
			this.label4.Location = new System.Drawing.Point(14, 105);
			this.label4.Name = "label4";
			this.label4.Size = new System.Drawing.Size(53, 12);
			this.label4.TabIndex = 6;
			this.label4.Text = "到时刻：";
			// 
			// btnSrcTimeNow
			// 
			this.btnSrcTimeNow.Location = new System.Drawing.Point(277, 17);
			this.btnSrcTimeNow.Name = "btnSrcTimeNow";
			this.btnSrcTimeNow.Size = new System.Drawing.Size(75, 23);
			this.btnSrcTimeNow.TabIndex = 8;
			this.btnSrcTimeNow.Text = "当前时刻";
			this.btnSrcTimeNow.UseVisualStyleBackColor = true;
			this.btnSrcTimeNow.Click += new System.EventHandler(this.btnSrcTimeNow_Click);
			// 
			// lblResult
			// 
			this.lblResult.AutoSize = true;
			this.lblResult.Location = new System.Drawing.Point(73, 105);
			this.lblResult.Name = "lblResult";
			this.lblResult.Size = new System.Drawing.Size(107, 12);
			this.lblResult.TabIndex = 9;
			this.lblResult.Text = "请点击“给我转”~";
			// 
			// btnConv
			// 
			this.btnConv.Location = new System.Drawing.Point(277, 98);
			this.btnConv.Name = "btnConv";
			this.btnConv.Size = new System.Drawing.Size(75, 23);
			this.btnConv.TabIndex = 10;
			this.btnConv.Text = "给我转";
			this.btnConv.UseVisualStyleBackColor = true;
			this.btnConv.Click += new System.EventHandler(this.btnConv_Click);
			// 
			// FormMain
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 12F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(549, 133);
			this.Controls.Add(this.btnConv);
			this.Controls.Add(this.lblResult);
			this.Controls.Add(this.btnSrcTimeNow);
			this.Controls.Add(this.label4);
			this.Controls.Add(this.cbDestTimeZone);
			this.Controls.Add(this.dtpSrcTime);
			this.Controls.Add(this.label3);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.label1);
			this.Controls.Add(this.cbSrcTimeZone);
			this.Name = "FormMain";
			this.Text = "时区转换";
			this.Load += new System.EventHandler(this.FormMain_Load);
			this.ResumeLayout(false);
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.ComboBox cbSrcTimeZone;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.DateTimePicker dtpSrcTime;
		private System.Windows.Forms.ComboBox cbDestTimeZone;
		private System.Windows.Forms.Label label4;
		private System.Windows.Forms.Button btnSrcTimeNow;
		private System.Windows.Forms.Label lblResult;
		private System.Windows.Forms.Button btnConv;
	}
}

