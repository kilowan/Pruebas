using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Web;
using WebApplicationMVC.Business;

namespace WebApplicationMVC.Models
{
    public class DriverHistory
    {
        public Driver driver { get; set; }
        [DisplayName("Año debut")]
        [Range(1950, 2019, ErrorMessage = "El valor para {0} debe estar entre {1} y {2}")]
        public int initial_year { get; set; }
        [DisplayName("Año retirada")]
        [Range(1950, 2019, ErrorMessage = "El valor para {0} debe estar entre {1} y {2}")]
        public int final_year { get; set; }
    }
}