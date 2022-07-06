using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mordecai.Types
{
    public class TwoWayExit : Exit
    {
        public Place Location2 { get; set; }
        public Place CurrentLocation { get; set; }

        public virtual Place GetDestination(IThing thing)
        {
            CurrentLocation = thing.Location;
            if(CurrentLocation == Location)
            {
                Destination = Location2;
            }
            else
            {
                Destination = Location;
            }
            return Destination;
        }
    }
}
