using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using WebApplicationMVC.Business;
using WebApplicationMVC.Models;

namespace WebApplicationMVC.Controllers
{
    public class GestionController : Controller
    {
        // GET: Gestion
        public ActionResult CrearEquipo()
        {
            return View();
        }
        // GET: Gestion
        [HttpPost]
        public ActionResult CrearEquipo(Team equipo)
        {
            TeamsManager teamsManager = new TeamsManager();
            if (!teamsManager.CreateTeam(equipo))
                ModelState.AddModelError("CustomError", "Ha habido un error.");
            return View(equipo);
        }
    }
}