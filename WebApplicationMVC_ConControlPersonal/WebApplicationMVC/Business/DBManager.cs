using System;
using System.Collections.Generic;
using System.Data;
using System.Data.OleDb;
using System.Data.SqlClient;
using System.Linq;
using System.Web;
using WebApplicationMVC.Models;

namespace WebApplicationMVC.Business
{
    public class DBManager
    {
        private OleDbConnection conn;
        public void ConnectDataBase()
        {
            this.conn = new OleDbConnection();
            //PC
            //string path = @"E:\Mis documentos";
            //Portatil
            //string path = @"C:\Users\juan\Documents";
            //OESIA
            string path = @"C:\Users\extjfnavarro\Documents";
            this.conn.ConnectionString = $@"Provider=Microsoft.ACE.OLEDB.12.0;Data source= {path}\DWN\OESIA\WebApplicationMVC_ConControlPersonal\F1.accdb";
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
                string query = $"INSERT INTO Equipos (Nombre, Fundacion, Grandes_Premios, Victorias, Mundiales) VALUES ('{equipo.Nombre}', {equipo.Fundacion},{equipo.Grandes_Premios},{equipo.Victorias},{equipo.Mundiales})";
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
        public PilotRooster GetPilotRoosterPerYear(int year)
        {
            PilotRooster pilotrooster = new PilotRooster();
            OleDbCommand command = new OleDbCommand();
            command.Connection = this.conn;
            command.CommandText = $@"SELECT p.*, pm.Puntos 
                FROM (Pilotos AS p INNER JOIN Pilotos_Mundial AS pm ON p.id = pm.Id_Piloto)
                INNER JOIN Mundial AS m ON pm.Id_MundialEquipo = m.Id 
                WHERE m.year = {year}";
            /*command.CommandText = $@"SELECT p.*, pm.Puntos 
                FROM Pilotos AS p, Pilotos_Mundial AS pm, Mundial as m
                WHERE p.id = pm.Id_Piloto AND pm.Id_MundialEquipo = m.Id AND m.year = { year}";*/
            OleDbDataReader reader = command.ExecuteReader();
            while (reader.Read())
            {
                Pilot pilot = new Pilot();
                pilot.Id = Convert.ToInt32(reader["Id"]);
                pilot.Name = reader["Nombre"].ToString();
                pilot.Surname = reader["Apellido"].ToString();
                pilot.Birth_date = reader["FechaNacimiento"].ToString();
                pilot.Nacionality = reader["Nacionalidad"].ToString();
                pilot.Points = Convert.ToInt32(reader["Puntos"]);
                pilotrooster.Pilot_Rooster.Add(pilot);
            }
            return pilotrooster;
        }
    }
}