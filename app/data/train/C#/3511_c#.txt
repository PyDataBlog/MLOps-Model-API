using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using AutoMapper;
using MvcAutoMapper.Controllers.Base;

namespace MvcAutoMapper.Controllers
{
    public class HomeController : BaseController
    {
        public HomeController(IMapper mapper) : base(mapper)
        {
        }

        public ActionResult Index()
        {
            return View();
        }

    }
}