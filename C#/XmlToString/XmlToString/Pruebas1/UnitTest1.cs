using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using XmlToString;

namespace Pruebas1
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            string path = @"C:\Users\jfnavarro\Downloads\documentación web service\documentación web service\Ejemplos de Respuesta\";
            string fichero1 = "XML Salida CTE Bustamante 20130614.xml";
            string fichero2 = "XML Salida CTE Ficticia.xml";
            string fichero3 = "XML Salida CTE Servimat 20160322.xml";

            string xml1 = Class1.GetData($@"{path}{fichero1}").ToString();
            string xml2 = Class1.GetData($@"{path}{fichero2}").ToString();
            string xml3 = Class1.GetData($@"{path}{fichero3}").ToString();
        }

        [TestMethod]
        public void SaveSQLFiles()
        {
            string database = "MONITORIZA_XX_DB";
            string instruction_Type = "IN";
            string day = DateTime.Now.Day.ToString("00");
            string month = DateTime.Now.Month.ToString("00");
            string completeyear = DateTime.Now.Year.ToString();
            string year = completeyear.Substring(2);

            string datefull = $"{year}{month}{day}";
            string table = "Monitoriza_TACOM_ApplicationSettings";
            string path = $@"C:\Users\jfnavarro\Desktop\PRO_{database}_{datefull}_{instruction_Type}_{table}.sql";
            string email = "jfnavarro@oesia.com";
            string description = "Insert SSO Urls to table [Common].[TACOM_ApplicationSettings]";
            string SQLCode = @"  INSERT INTO [Common].[TACOM_ApplicationSettings] VALUES( NULL, 1, 'URLAlertsEntitySSO', 'https://axesor360.axesor.com/sso/[domain]?ir_a=seguimiento&seccion=gestion&entidad=[EntityId]&tab=alertas', 'Url de alertas SSO de usuario');
  
  INSERT INTO [Common].[TACOM_ApplicationSettings] VALUES( NULL, 1, 'URLAlertsGroupsSSO', 'https://axesor360.axesor.com/sso/[domain]?ir_a=seguimiento/sociedades', 'Url de los grupos de alertas SSO');";
            Class1.SaveSQLFile(path, email, description, database, SQLCode);
        }
    }
}
