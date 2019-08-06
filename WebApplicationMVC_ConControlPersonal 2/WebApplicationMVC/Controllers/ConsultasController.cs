using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using WebApplicationMVC.Business;
using WebApplicationMVC.Models;

namespace WebApplicationMVC.Controllers
{
    public class ConsultasController : Controller
    {
        // GET: Consultas
        [HttpPost]
        public ActionResult RoosterYear(int year)
        {
            TeamsManager teamsManager = new TeamsManager();
            Rooster rooster = teamsManager.GetTeams(year);
            return View(rooster);
        }

        [HttpGet]
        public ActionResult RoosterYear()
        {
            Rooster rooster = new Rooster();
            rooster.TeamRooster = new List<Team>();
            return View(rooster);
        }
        // GET: Consultas
        [HttpPost]
        public ActionResult DriversYear(int year)
        {
            DriversManager driversManager = new DriversManager();
            ChampsionshipDrivers rooster = driversManager.GetDrivers(year);
            return View(rooster);
        }

        [HttpGet]
        public ActionResult DriversYear()
        {
            ChampsionshipDrivers rooster = new ChampsionshipDrivers();
            rooster.DriversRooster = new List<Driver>();
            return View(rooster);
        }
    }
}