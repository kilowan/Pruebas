using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace WebApplicationMVC.Models
{
    public class ChampsionshipDrivers
    {
        public Season Season { get; set; }
        public List<Driver> DriversRooster { get; set; }
        public ChampsionshipDrivers()
        {
            Season = new Season();
            DriversRooster = new List<Driver>();
        }

        public ChampsionshipDrivers(List<Driver> driverRooster)
        {
            DriversRooster = driverRooster;
        }
    }
}