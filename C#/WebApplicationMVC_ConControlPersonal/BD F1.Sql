CREATE TABLE Equipos(
Id INT PRIMARY KEY IDENTITY,
Nombre VARCHAR(100),
Fundacion INT,
Grandes_Premios int,
Victorias INT,
Mundiales INT
);
CREATE TABLE Mundial(
Id INT PRIMARY KEY IDENTITY,
Year INT
);
CREATE TABLE Pilotos(
Id INT PRIMARY KEY IDENTITY,
Nombre VARCHAR(100),
Apellido VARCHAR(100),
FechaNacimiento VARCHAR(100),
Nacionalidad VARCHAR(100)
);
CREATE TABLE Mundial_Equipos(
Id INT PRIMARY KEY IDENTITY,
Id_Equipo INT,
Id_Mundial INT
CONSTRAINT FK_Id_Equipo FOREIGN KEY (Id_Equipo) REFERENCES Equipos (Id),
CONSTRAINT FK_Id_Mundial FOREIGN KEY (Id_Mundial) REFERENCES Mundial (Id)
);
CREATE TABLE Pilotos_Mundial(
Id INT PRIMARY KEY IDENTITY,
Id_Piloto INT,
ID_MundialEquipo INT,
Puntos INT
CONSTRAINT FK_Id_Piloto FOREIGN KEY (Id_Piloto) REFERENCES Pilotos (Id),
CONSTRAINT FK_Id_MundialEquipo FOREIGN KEY (Id_MundialEquipo) REFERENCES Mundial_Equipos (Id)
);