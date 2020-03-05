using System;
using System.Collections.Generic;
using System.Data.OleDb;
using System.Linq;
using System.Web;
using System.Diagnostics;
using WebApplicationMVC.Models;
using System.Data;
using System.Data.SqlClient;

namespace WebApplicationMVC.Business
{
    public class DBManager
    {
        private OleDbConnection conn;
        private OleDbDataReader reader;

        public void ConnectDataBase()
        {
            this.conn = new OleDbConnection();
            this.conn.ConnectionString = $@"Provider=Microsoft.ACE.OLEDB.12.0;Data source= {new ProcessStartInfo("cmd.exe", "/K " + "%USERPROFILE%").EnvironmentVariables["USERPROFILE"]}\Documents\DWN\OESIA\WebApplicationMVC_ConControlPersonal 2\F1.accdb";
            this.conn.Open();
        }
        public void DisconnectDataBase()
        {
            this.conn.Close();
        }

        public Rooster GetRoosterPerYear(int year)
        {
            string query = "SELECT e.* FROM (Equipos AS e INNER JOIN Mundial_Equipos AS me ON e.Id = me.Id_Equipo) INNER JOIN Mundial AS m ON me.Id_Mundial = m.Id WHERE m.Year = " + year;
            OleDbCommand command = new OleDbCommand();
            command.Connection = this.conn;
            command.CommandText = query;
            OleDbDataReader reader = command.ExecuteReader();
            Rooster result = new Rooster();
            while (reader.Read())
            {
                Team team = new Team();
                team.Id = Convert.ToInt32(reader["Id"]);
                team.Fundacion = Convert.ToInt32(reader["Fundacion"]);
                team.Grandes_Premios = Convert.ToInt32(reader["Grandes_Premios"]);
                team.Mundiales = Convert.ToInt32(reader["Mundiales"]);
                team.Nombre = reader["Nombre"].ToString();
                team.Victorias = Convert.ToInt32(reader["Victorias"]);
                result.TeamRooster.Add(team);
            }

            return result;
        }

        public List<Season> GetWorldChampionShips()
        {
            string query = "SELECT * FROM Mundial";
            OleDbCommand command = new OleDbCommand();
            command.Connection = this.conn;
            command.CommandText = query;
            OleDbDataReader reader = command.ExecuteReader();
            List<Season> result = new List<Season>();
            while (reader.Read())
            {
                Season season = new Season();
                season.Year = Convert.ToInt32(reader["Year"]);
                season.Id = Convert.ToInt32(reader["Id"]);
                result.Add(season);
            }

            return result;
        }

        public bool CreateTeam(Team equipo)
        {
            try
            {
                string query = $"NSERT INTO Equipos (Nombre, Fundacion, Grandes_Premios, Victorias, Mundiales) VALUES ('{equipo.Nombre}', {equipo.Fundacion},{equipo.Grandes_Premios},{equipo.Victorias},{equipo.Mundiales})";
                OleDbCommand command = new OleDbCommand();
                command.Connection = this.conn;
                command.CommandText = query;
                command.ExecuteNonQuery();

                return true;
            }
            catch (Exception ex)
            {
                return false;
            }
        }
        public bool CreateDriver(Driver piloto, int año_inicial, int año_final)
        {
            bool ok = false;
            string query = $@"INSERT INTO Pilotos (Nombre, Apellido, FechaNacimiento, Nacionalidad
                ) VALUES ('{piloto.Nombre}', '{piloto.Apellido}', '{piloto.FechaNacimiento}', '{piloto.Nacionalidad}')";
            OleDbCommand command = new OleDbCommand();
            command.Connection = this.conn;
            command.CommandText = query;
            if (command.ExecuteNonQuery() == 1)
            {
                query = $@"SELECT Id FROM Pilotos 
                    WHERE 
                        Nombre = '{piloto.Nombre}' 
                    AND 
                        Apellido = '{piloto.Apellido}' 
                    AND 
                        FechaNacimiento = '{piloto.FechaNacimiento}'
                    AND Nacionalidad = '{piloto.Nacionalidad}'";
                command.CommandText = query;
                this.reader = command.ExecuteReader();
                this.reader.Read();
                int id_piloto = Convert.ToInt32(this.reader["Id"]);
                this.reader.Close();
                IList<int> Id_list = new List<int>();
                query = $"SELECT Id FROM Mundial WHERE Year BETWEEN {año_inicial} AND {año_final}";
                command.CommandText = query;
                this.reader = command.ExecuteReader();
                while(this.reader.Read())
                {
                    Id_list.Add(Convert.ToInt32(this.reader["Id"]));
                }
                this.reader.Close();
                foreach (int id in Id_list)
                {
                    query = $"INSERT INTO Pilotos_Mundial (Id_Piloto, Id_MundialEquipo, Puntos) VALUES ({id_piloto}, {id}, 0)";
                    command.CommandText = query;
                    if (command.ExecuteNonQuery() != 1)
                    {
                        ok =  false;
                    }
                    else
                    {
                        ok = true;
                    }
                }
            }
            return ok;

        }

        public ChampsionshipDrivers GetDriversPerYear(int year)
        {
            string query = "SELECT p.*, pm.Puntos FROM (Pilotos AS p INNER JOIN Pilotos_Mundial AS pm ON p.Id = pm.Id_Piloto) INNER JOIN Mundial AS m ON pm.Id_MundialEquipo = m.Id WHERE m.Year = " + year;
            OleDbCommand command = new OleDbCommand();
            command.Connection = this.conn;
            command.CommandText = query;
            OleDbDataReader reader = command.ExecuteReader();
            ChampsionshipDrivers result = new ChampsionshipDrivers();
            while (reader.Read())
            {
                Driver driver = new Driver();
                driver.Id = Convert.ToInt32(reader["Id"]);
                driver.Nombre = reader["Nombre"].ToString();
                driver.Apellido = reader["Apellido"].ToString();
                driver.FechaNacimiento = reader["FechaNacimiento"].ToString();
                driver.Nacionalidad = reader["Nacionalidad"].ToString();
                driver.Puntos = Convert.ToInt32(reader["Puntos"]);
                result.DriversRooster.Add(driver);
            }

            return result;
        }
    }
}