using ExcelDataReader;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Xml.Linq;

namespace XmlToString
{
    public class Class1
    {
        public static string GetData(string ruta)
        {
            // load the file using;
            XDocument xDocument = XDocument.Load(ruta);
            // convert the xml into string
            string xml = xDocument.ToString();
            return xml;
        }
        public static IDictionary<string, object> GetTags(string path)
        {
            IDictionary<string, object> Data = new Dictionary<string, object>();
            using (var stream = File.Open(path, FileMode.Open, FileAccess.Read))
            {
                using (var reader = ExcelReaderFactory.CreateReader(stream))
                {
                    int counter = 0;
                    do
                    {
                        while (reader.Read())
                        {
                            Data.Add(reader.GetName(counter), reader.GetValue(counter));
                            counter++;
                        }
                    } while (reader.NextResult())
  ;
                }
            }
            return Data;
            //String file = File.ReadAllText(@"C:\Users\juan\Downloads\Etiquetas\TaxTags.html");
            //file.g("$.i18n.t(\"");
            //Regex.Matches(file, @"\"(")
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

        private static void SaveToExcel()
        {
            
            IList<string> strings = new List<string>();

        }
    }
}
