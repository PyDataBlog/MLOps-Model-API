using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace SistemaMundoAnimal.Forms {
    public partial class FormPrincipal : Form {

        public FormPrincipal() {
            InitializeComponent();
        }

        public void ShowVisualizarFuncionario(int codigo) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormVisualizarFuncionario(codigo));
        }

        public void ShowVisualizarProduto(int codigo) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormVisualizarProduto(codigo));
        }

        private void BtnCadastrarFuncionario_Click(object sender, EventArgs e) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormCadastroFuncionario());
        }

        private void BtnPesquisaFuncionario_Click(object sender, EventArgs e) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormPesquisarFuncionarios());
        }

        private void BtnVisualizarFuncionario_Click(object sender, EventArgs e) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormVisualizarFuncionario());
        }

        private void BtnCadastrarProduto_Click(object sender, EventArgs e) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormCadastroProduto());
        }

        private void BtnPesquisarProduto_Click(object sender, EventArgs e) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormPesquisarProduto());
        }

        private void BtnVisualizarProduto_Click(object sender, EventArgs e) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormVisualizarProduto());
        }

        private void BtnEstoque_Click(object sender, EventArgs e) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormEstoque());
        }

        private void BtnPesquisarVenda_Click(object sender, EventArgs e) {
            PainelPrincipal.Controls.Clear();
            PainelPrincipal.Controls.Add(new FormPesquisarVenda());
        }

    }
}
