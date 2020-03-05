using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace WebApplicationMVC.Models
{
    public class Pilot
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string Surname { get; set; }
        public string Birth_date { get; set; }
        public string Nacionality { get; set; }
        public int Points { get; set; }
    }
}