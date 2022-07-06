namespace Geometry
{
    using System;

    public class Rectangle
    {
        private double width;
        private double height;

        public Rectangle(double rectangleWidth, double rectangleHeight)
        {
            this.Width = rectangleWidth;
            this.Height = rectangleHeight;
        }

        public double Width
        {
            get
            {
                return this.width;
            }

            private set
            {
                if (value <= 0)
                {
                    throw new ArgumentException("Width can not be less or equal to 0!");
                }

                this.width = value;
            }
        }

        public double Height
        {
            get
            {
                return this.height;
            }

            private set
            {
                if (value <= 0)
                {
                    throw new ArgumentException("Height can not be less or equal to 0!");
                }

                this.height = value;
            }
        }
    }
}
