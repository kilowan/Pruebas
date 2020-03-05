using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;

namespace XmlToString
{
    public class Class1
    {
        public static string GetData(string ruta)
        {
            // load the file using;
            var xDocument = XDocument.Load(ruta);
            // convert the xml into string
            string xml = xDocument.ToString();
            return xml;
        }

        public static void SaveSQLFile(string path, string email, string description, string database, string SQLCode)
        {
            string content = Create_string(email, description, database, SQLCode);
            StreamWriter sw = File.CreateText(path);
            sw.Write(content);
            sw.Close();
        }

        private static string Create_string(string email, string description, string database, string SQLCode)
        {
            int year = DateTime.Now.Year;
            string month = DateTime.Now.Month.ToString("00");
            string day = DateTime.Now.Day.ToString("00");
            return $@"-- =============================================
-- Author: {email}
 -- Create date: {day} - {month} - {year}
 -- Description: {description}
 -- Requires: Connection to {database}
 -- =============================================

{SQLCode}";
            
        }
    }
}
