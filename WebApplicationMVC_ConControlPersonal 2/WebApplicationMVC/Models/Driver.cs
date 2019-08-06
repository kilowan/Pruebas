using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace WebApplicationMVC.Models
{
    public class Driver
    {
        public int Id { get; set; }
        public string Nombre { get; set; }
        public string Apellido {get;set;}
        public string FechaNacimiento { get; set; }
        public string Nacionalidad { get; set; }
        public int Puntos { get; set; }
    }
}