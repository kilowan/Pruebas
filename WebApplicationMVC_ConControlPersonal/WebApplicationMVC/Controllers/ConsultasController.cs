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
        [HttpPost]
        public ActionResult PilotsYear(int year)
        {
            PilotsManager pilotsManager = new PilotsManager();
            PilotRooster pilotrooster = pilotsManager.GetPilots(year);
            return View(pilotrooster);
        }

        // POST: Consultas
        [HttpGet]
        public ActionResult RoosterYear()
        {
            Rooster rooster = new Rooster();
            rooster.TeamRooster = new List<Team>();
            return View(rooster);
        }
        [HttpGet]
        public ActionResult PilotsYear()
        {
            PilotRooster pilotrooster = new PilotRooster();
            pilotrooster.Pilot_Rooster = new List<Pilot>();
            return View(pilotrooster);
        }
    }
}