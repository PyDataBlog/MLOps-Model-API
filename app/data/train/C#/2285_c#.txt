using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Data;

namespace BLL
{
     public partial class Task
    {
        DAL.Task pr = new DAL.Task();

       
        public DataTable GetTaskList(string strWhere)
        {
            return pr.GetTaskList(strWhere);
        }
        public bool AddTaskList(Model.Task model)
        {
            return pr.AddTaskList(model);
        }
        public bool UpdateTaskList(Model.Task model)
        {
            return pr.UpdateTaskList(model);
        }
    }
}
