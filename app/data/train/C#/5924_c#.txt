namespace ButcherBlock
{
    partial class PriceForm
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
            this.myListView = new System.Windows.Forms.ListView();
            this.addButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // myListView
            // 
            this.myListView.Font = new System.Drawing.Font("Segoe UI", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.myListView.FullRowSelect = true;
            this.myListView.GridLines = true;
            this.myListView.Location = new System.Drawing.Point(29, 25);
            this.myListView.Name = "myListView";
            this.myListView.Size = new System.Drawing.Size(812, 1094);
            this.myListView.TabIndex = 4;
            this.myListView.UseCompatibleStateImageBehavior = false;
            this.myListView.View = System.Windows.Forms.View.Details;
            this.myListView.DoubleClick += new System.EventHandler(this.myListView_Click);
            // 
            // addButton
            // 
            this.addButton.Font = new System.Drawing.Font("Segoe UI", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.addButton.Location = new System.Drawing.Point(877, 25);
            this.addButton.Name = "addButton";
            this.addButton.Size = new System.Drawing.Size(145, 66);
            this.addButton.TabIndex = 5;
            this.addButton.Text = "Add";
            this.addButton.UseVisualStyleBackColor = true;
            this.addButton.Click += new System.EventHandler(this.addButton_Click);
            // 
            // PriceForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1051, 1145);
            this.Controls.Add(this.addButton);
            this.Controls.Add(this.myListView);
            this.Name = "PriceForm";
            this.Text = "Product Prices";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ListView myListView;
        private System.Windows.Forms.Button addButton;

    }
}