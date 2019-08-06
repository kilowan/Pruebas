using System;
using Microsoft.SharePoint.Client;

namespace SHP
{
    class Program
    {
        static void Main(string[] args)
        {
            ClientContext context = new ClientContext("http://pruebabeca2019/beca/kilowan/");
            Web web = context.Web;
            context.Load(web);
            context.ExecuteQuery();
            Console.WriteLine(web.Title);
        }
    }
}
