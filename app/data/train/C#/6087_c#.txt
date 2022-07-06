using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using Windows.ApplicationModel;
using Windows.Web.Syndication;
using UwpIntro.Annotations;

namespace UwpIntro.ViewModels
{
    public class NewsItemVmi
    {
        public string Title { get; set; }
        public string Published { get; set; }
        public string Image { get; set; }
    }

    public class MainPageVm : INotifyPropertyChanged
    {
        private TimeSpan _time;

        public MainPageVm()
        {
            NewsItems = new ObservableCollection<NewsItemVmi>();

            Time = DateTime.Now.TimeOfDay;

            if (DesignMode.DesignModeEnabled)
            {
                NewsItems.Add(new NewsItemVmi { Title = "item 1", Published = "2015-01-01", Image = "/Assets/StoreLogo.png" });
                NewsItems.Add(new NewsItemVmi { Title = "item 2", Published = "2015-01-01", Image = "/Assets/StoreLogo.png" });
                NewsItems.Add(new NewsItemVmi
                {
                    Title = "item 3 with longer name",
                    Published = "2015-01-01",
                    Image = "/Assets/StoreLogo.png"
                });
                NewsItems.Add(new NewsItemVmi
                {
                    Title = "item 4 (design time only)",
                    Published = "2015-01-01",
                    Image = "/Assets/StoreLogo.png"
                });

            }
        }

        public async Task Init()
        {
            var client = new SyndicationClient();
            var feed = await client.RetrieveFeedAsync(
                new Uri("http://rss.cnn.com/rss/edition_technology.rss", UriKind.Absolute));

            foreach (var item in feed.Items)
            {
                var element = item.
                    ElementExtensions.FirstOrDefault(e => e.NodeName == "thumbnail");
                var attribute = element.AttributeExtensions.FirstOrDefault(ae => ae.Name == "url");
                var uri = attribute?.Value;
                var imageUri = uri ?? "https://pbs.twimg.com/profile_images/508960761826131968/LnvhR8ED.png";
                NewsItems.Add(new NewsItemVmi
                {
                    Title = item.Title.Text,
                    Image = imageUri,
                    Published = item.PublishedDate.ToString("g")
                });
            }
        }
        public ObservableCollection<NewsItemVmi> NewsItems { get; set; }

        public TimeSpan Time
        {
            get { return _time; }
            set { _time = value; OnPropertyChanged();}
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
