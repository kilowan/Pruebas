using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Web;

namespace WebApplicationMVC.Models
{
    public class Team
    {
        public int Id { get; set; }
        public string Nombre { get; set; }
        [DisplayName("Fundación")]
        [Range(1950, 2019, ErrorMessage = "El valor para {0} debe estar entre {1} y {2}")]
        public int Fundacion { get; set; }
        [DisplayName("Grandes Premios")]
        public int Grandes_Premios { get; set; }
        public int Victorias { get; set; }
        public int Mundiales { get; set; }
    }
}