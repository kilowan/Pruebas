using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using WebApplicationMVC.Models;

namespace WebApplicationMVC.Business
{
    public class PilotsManager
    {
        public PilotRooster GetPilots(int year)
        {
            DBManager dbManager = new DBManager();
            dbManager.ConnectDataBase();
            PilotRooster result = dbManager.GetPilotRoosterPerYear(year);
            result.Season.Year = year;
            dbManager.DisconnectDataBase();
            return result;
        }
    }
}