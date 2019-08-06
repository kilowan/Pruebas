using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using WebApplicationMVC.Models;

namespace WebApplicationMVC.Business
{
    public class DriversManager
    {
        private DBManager dBManager;
        private bool ok;

        public ChampsionshipDrivers GetDrivers(int year)
        {
            DBManager dbManager = new DBManager();
            dbManager.ConnectDataBase();
            ChampsionshipDrivers result = dbManager.GetDriversPerYear(year);
            result.Season.Year = year;
            dbManager.DisconnectDataBase();
            return result;
        }
        public bool create_Driver(DriverHistory driverHistory)
        {
            this.dBManager = new DBManager();
            this.dBManager.ConnectDataBase();
            this.ok = this.dBManager.CreateDriver(driverHistory.driver, driverHistory.initial_year, driverHistory.final_year);
            this.dBManager.DisconnectDataBase();
            return this.ok;
        }
    }
}