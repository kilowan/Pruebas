using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace WebApplicationMVC.Models
{
    public class Team
    {
        public int Id { get; set; }
        public string Nombre { get; set; }
        public int Fundacion { get; set; }
        public int Grandes_Premios { get; set; }
        public int Victorias { get; set; }
        public int Mundiales { get; set; }
    }
}