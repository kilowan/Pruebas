using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using WebApplicationMVC.Models;

namespace WebApplicationMVC.Business
{
    public class GenericManager
    {
        public List<Season> GetYears()
        {
            DBManager dbManager = new DBManager();
            dbManager.ConnectDataBase();
            List<Season> result = dbManager.GetWorldChampionShips();
            dbManager.DisconnectDataBase();
            return result;
        }
    }
}