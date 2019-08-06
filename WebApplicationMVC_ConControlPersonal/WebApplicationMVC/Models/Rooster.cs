using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace WebApplicationMVC.Models
{
    public class Rooster
    {
        public Season Season { get; set; }
        public List<Team> TeamRooster { get; set; }
        public Rooster()
        {
            this.Season = new Season();
            this.TeamRooster = new List<Team>();
        }

        public Rooster(List<Team> teamRooster)
        {
            this.TeamRooster = teamRooster;
        }
    }
}