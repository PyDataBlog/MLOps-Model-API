using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SchoolDB.Models
{
  /// <summary>
  /// 
  /// </summary>
  public class Major
  {
    public int MajorId { get; set; }
    public string MajorName { get; set; }
    public bool Active { get; set; }
  }
}
