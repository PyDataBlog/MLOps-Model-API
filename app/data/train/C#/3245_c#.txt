
namespace Shape
{
    using System;

    public abstract class Shape
    {
        private double width;
        private double height;

        //public Shape(double parameter)
        //{
        //    this.Width = parameter;
        //    this.Height = parameter;
        //}
        public Shape(double width, double height)
        {
            this.Width = width;
            this.Height = height;
        }
        public double Width
        {
            get
            {
                return this.width;
            }

            set
            {
                if (value <= 0)
                {
                    throw new ArgumentException("The value of Width cannot be 0 or negative.");
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

            set
            {
                if (value <= 0)
                {
                    throw new ArgumentException("The value of Height cannot be 0 or negative.");
                }

                this.height = value;
            }
        }


        public virtual double CalculateSurface()
        {
            return 0.0;
        }
    }
}
