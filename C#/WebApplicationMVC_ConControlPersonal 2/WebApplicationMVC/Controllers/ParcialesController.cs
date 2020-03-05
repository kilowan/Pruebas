using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using WebApplicationMVC.Business;
using WebApplicationMVC.Models;

namespace WebApplicationMVC.Controllers
{
    public class ParcialesController : Controller
    {
        // GET: Parciales
        public ActionResult Index()
        {
            return View();
        }

        [HttpPost]
        public ActionResult AltaEquipo(Team t)
        {
            return View();
        }

        [HttpPost]
        public ActionResult AltaPiloto(Driver d)
        {
            return View("Index");
        }

        [HttpPost]
        public ActionResult Index(Team t, Driver d)
        {
            TeamsManager teamsManager = new TeamsManager();
            if (!teamsManager.CreateTeam(t))
                ModelState.AddModelError("CustomError", "Ha habido un error.");
            return View("Index");
        }

        [ChildActionOnly]
        public ActionResult Driver()
        {
            Driver model = new Driver();
            return PartialView("_Driver", model);
        }

        [ChildActionOnly]
        public ActionResult Team()
        {
            Team model = new Team();
            return PartialView("_Team", model);
        }
    }
}