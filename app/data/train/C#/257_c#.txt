namespace StudentClass
{
    public class Course
    {
        public Course(string name)
            : this()
        {
            this.Name = name;
        }

        public Course()
        {

        }

        public string Name { get; private set; }

        public override string ToString()
        {
            return this.Name;
        }
    }
}
