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
        private DriversManager driversManager;
        // GET: Gestion
        [HttpGet]
        public ActionResult CrearEquipo()
        {
            return View();
        }
        [HttpGet]
        public ActionResult CrearPiloto()
        {
            return View();
        }
        // POST: Gestion
        [HttpPost]
        public ActionResult CrearEquipo(Team equipo)
        {
            if (this.ModelState.IsValid)
            {
                TeamsManager teamsManager = new TeamsManager();
                if (!teamsManager.CreateTeam(equipo))
                    this.ModelState.AddModelError("CustomError", "Ha habido un error.");
                return View(equipo);
            }
            else
                return View(equipo);
        }
        [HttpPost]
        public ActionResult CrearPiloto(DriverHistory driverHistory)
        {
            if (this.ModelState.IsValid)
            {
                this.driversManager = new DriversManager();
                if (!this.driversManager.create_Driver(driverHistory))
                    this.ModelState.AddModelError("CustomError", "Ha habido un error.");
                return View(driverHistory);
            }
            else
                return View(driverHistory);
        }
    }
}