using ClassLibrary;
using Pokemon_IMIE.Model;
using Pokemon_IMIE.usercontrols;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices.WindowsRuntime;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Navigation;

// The User Control item template is documented at http://go.microsoft.com/fwlink/?LinkId=234236

namespace Pokemon_IMIE.usercontrols
{
    public sealed partial class PokemonStatus : BaseUserControl
    {

        private Model.Pokemon pokemon;

        public Model.Pokemon Pokemon
        {
            get { return pokemon; }
            set
            {
                pokemon = value;
                base.OnPropertyChanged("Pokemon");
            }
        }

        public PokemonStatus()
        {
            this.InitializeComponent();
            base.DataContext = this;
        }
    }
}
