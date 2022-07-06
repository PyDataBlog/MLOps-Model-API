using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TravelingSalesmanProblem.Engine
{
    public class Point
    {
        /* Class used as a point. Represent a location/gene in the path. */

        #region Fields
        public int X; /* Variables used to display the path on the screen */
        public int Y;
        public bool Visited; /* Boolean ; Is the point visited or not */
        #endregion

        #region Constructors
        public Point(int X, int Y)
        { 
            /* Class Constructor */
            this.X = X;
            this.Y = Y;
        }
        #endregion

        #region Properties
        public System.Drawing.Point DrawingPoint
        {
            get { return new System.Drawing.Point(this.X, this.Y); } /* Convert this point into Drawing.Point */
        }
        #endregion

        #region Public Methods
        public void Visit()
        {
            /* Visit the point */
            this.Visited = true;
        }

        public int GetDistance(Point other)
        {
            /* Return the distance between this point and another */
            int diffX = X - other.X;
            int diffY = Y - other.Y;
            return diffX * diffX + diffY * diffY;
        }
        #endregion
    }
}
