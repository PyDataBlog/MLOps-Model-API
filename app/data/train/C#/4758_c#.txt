using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices.WindowsRuntime;
using Darts.UI.View.Generic;
using Darts.UI.View.Pages;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Navigation;

// The Blank Page item template is documented at http://go.microsoft.com/fwlink/?LinkId=402352&clcid=0x409

namespace Darts.UI
{
    /// <summary>
    /// An empty page that can be used on its own or navigated to within a Frame.
    /// </summary>
    public sealed partial class MainPage : Page
    {
        public MainPage()
        {
            this.InitializeComponent();

            Pages = new List<PageDescriptor> { new PageDescriptor( "Home", typeof( HomePage ) ), new PageDescriptor( "Select Game", typeof( SelectGamePage ) ) };
            CurrentPage = Pages.First();
        }

        protected override void OnNavigatedTo( NavigationEventArgs e )
        {
            base.OnNavigatedTo( e );
        }

        private void Button_Click( object sender, RoutedEventArgs e )
        {
            Splitter.IsPaneOpen = !Splitter.IsPaneOpen;
        }

        private void ScenarioControl_SelectionChanged( object sender, SelectionChangedEventArgs e )
        {
            Splitter.IsPaneOpen = false;
            var theNewPage = e.AddedItems.FirstOrDefault() as PageDescriptor;
            if( theNewPage != null )
            {
                // new page requested, navigate
                CurrentPage = e.AddedItems.FirstOrDefault() as PageDescriptor;
                NavigationFrame.Navigate( CurrentPage.PageType );
            }
            else
            {
                // reset highlight if user tried de-selecting
                ( sender as ListBox ).SelectedItem = CurrentPage;
            }
        }




        public PageDescriptor CurrentPage
        {
            get
            {
                return ( PageDescriptor )GetValue( CurrentPageProperty );
            }
            set
            {
                SetValue( CurrentPageProperty, value );
            }
        }
        public static readonly DependencyProperty CurrentPageProperty =
            DependencyProperty.Register( "CurrentPage", typeof( PageDescriptor ), typeof( MainPage ), new PropertyMetadata( 0 ) );



        IEnumerable<PageDescriptor> Pages{ get; }
    }
}
