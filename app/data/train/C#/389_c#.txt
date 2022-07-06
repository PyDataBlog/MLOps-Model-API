using System;
using System.Linq;
using System.Reflection;
using System.Text;

namespace Stealer
{
    public class Spy
    {
        public string StealFieldInfo(string className, params string[] fieldsNames)
        {
            var sb = new StringBuilder();

            sb.AppendLine($"Class under investigation: {className}");

            var type = typeof(Hacker);

            var classInstance = Activator.CreateInstance(type, new object[] { });

            var fieldsInfo = type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);

            foreach (FieldInfo field in fieldsInfo.Where(fld => fieldsNames.Contains(fld.Name)))
            {
                sb.AppendLine($"{field.Name} = {field.GetValue(classInstance)}");
            }

            return sb.ToString();
        }
    }
}
