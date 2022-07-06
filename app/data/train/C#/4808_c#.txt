using Xamarin.Forms;

namespace XSvgPath
{
    public partial class ViaXamlView : ContentPage
    {
        public ViaXamlView()
        {
            InitializeComponent();
            BindingContext = new ViaXamlViewModel();
        }
    }
}
