using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace WebApplicationMVC.Models
{
    public class PilotRooster
    {
        public Season Season { get; set; }
        public List<Pilot> Pilot_Rooster { get; set; }
        public PilotRooster()
        {
            this.Season = new Season();
            this.Pilot_Rooster = new List<Pilot>();
        }

        public PilotRooster(List<Pilot> pilotRooster)
        {
            this.Pilot_Rooster = pilotRooster;
        }
    }
}