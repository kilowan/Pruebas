using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using WebApplicationMVC.Models;

namespace WebApplicationMVC.Business
{
    public class TeamsManager
    {
        public Rooster GetTeams(int year)
        {
            DBManager dbManager = new DBManager();
            dbManager.ConnectDataBase();
            Rooster result = dbManager.GetRoosterPerYear(year);
            result.Season.Year = year;
            dbManager.DisconnectDataBase();
            return result;
        }
        public bool CreateTeam(Team equipo)
        {
            DBManager dbManager = new DBManager();
            dbManager.ConnectDataBase();
            bool result = dbManager.CreateTeam(equipo);
            dbManager.DisconnectDataBase();
            return result;
        }
    }
}