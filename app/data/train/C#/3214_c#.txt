namespace Shapes
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Threading.Tasks;

    public class Square : Shape
    {
        public Square(double sides)
            :base (sides,sides)
        {

        }

        public override double CalculateSurface()
        {
            return this.Width * this.Height;
        }
    }
}
