--tema 3
--TablAS EMPLE y DEPART
--01 - SelecciONa el apellido, el oficio y la localidad de los departamentos de aquellos empleados cuyo oficio sea "ANALISTA".
SELECT apellido, oficio, loc 
FROM emple NATURAL JOIN depart 
WHERE oficio = 'ANALISTA'

--02 - Obtén los datos de los empleados cuyo director (columna DIR de la tabla EMPLE) sea "CEREZO".
SELECT * 
FROM emple 
WHERE dir = (
	SELECT emp_no 
	FROM emple 
	WHERE apellido = 'CEREZO');

--03 - Obtén los datos de los empleados del departamento de "VENTAS".
SELECT * 
FROM depart 
NATURAL JOIN emple 
WHERE dnombre = 'VENTAS';

--04 - Obtén los datos de los departamentos que NO tengan empleados.
SELECT * 
FROM depart 
WHERE dept_no 
NOT IN (
	SELECT dept_no 
	FROM emple
	);
	
--05 - Obtén los datos de los departamentos que tengan empleados.
SELECT * 
FROM depart 
WHERE dept_no
IN (
	SELECT dept_no 
	FROM emple
	);
	
--06 - Obtén el apellido y el salario de los empleados que superen todos los salarios de los empleados del departamento 20.	
SELECT apellido, salario 
FROM emple 
WHERE salario > (
	SELECT MAX(salario) 
	FROM emple 
	WHERE dept_no = 20);

--Tabla LIBRERIA	
--07 - VISualiza el tema, estante y ejemplares de lAS filAS de LIBRERIA cON ejemplares comprendidos entre 8 y 15.	
SELECT * 
FROM libreria 
WHERE ejemplares 
BETWEEN 8 AND 15 
ORDER BY ejemplares ASC;

--08 - VISualiza lAS columnAS TEMA, ESTANTE y EJEMPLARES de lAS filAS cuyo ESTANTE no esté comprendido entre la "B" y la "D".
SELECT * 
FROM libreria 
WHERE estante 
NOT BETWEEN 'B' AND 'D' 
ORDER BY estante ASC;

--09 - VISualiza cON una sola orden SELECT todos los temAS de LIBRERIA cuyo número de ejemplares sea INferior a los que hay en "MEDICINA".
SELECT tema 
FROM libreria 
WHERE ejemplares > (
	SELECT ejemplares 
	FROM libreria 
	WHERE tema = 'MEDICINA'
);

--10 - VISualiza los temAS de LIBRERIA cuyo número de ejemplares no esté entre 15 y 20, ambos INcluidos.
SELECT tema 
FROM libreria 
WHERE ejemplares 
NOT BETWEEN 15 AND 20;

--TablAS ALUMNOS, ASIGNATURAS y NOTAS
--11 - VISualiza todAS lAS ASignaturAS que cONtengan tres letrAS "o" en su INterior y tengan alumnos matriculados de "Madrid".
SELECT * 
FROM ASignaturAS NATURAL JOIN alumnos
WHERE nombre 
LIKE '%o%o%o%' AND pobla = 'Madrid';

--12 - VISualiza los nombres de alumnos de "Madrid" que tengan alguna ASignatura suspendida.
SELECT apenom 
FROM alumnos 
NATURAL JOIN NOTAS 
NATURAL JOIN ASignaturAS 
WHERE pobla = 'Madrid' AND NOTa <5;

--13 - Muestra los nombres de alumnos que tengan la mISma NOTa que tiene "Díaz Fernández, María" en "FOL" en alguna ASignatura.
SELECT * 
FROM alumnos 
NATURAL JOIN ASignaturAS 
NATURAL JOIN NOTAS 
WHERE NOTa = (
	SELECT NOTa 
	FROM NOTAS 
	NATURAL JOIN ASignaturAS 
	NATURAL JOIN alumnos 
	WHERE apenom = 'Díaz Fernández, María' 
	AND nombre = 'FOL') 
AND apenom != 'Díaz Fernández, María';

--14 - Obtén los datos de lAS ASignaturAS que no tengan alumnos.
SELECT cod 
FROM ASignaturAS 
WHERE cod 
NOT IN (
	SELECT cod 
	FROM NOTAS
);

--15 - Obtén el nombre y apellido de los alumnos que tengan NOTa en la ASignatura cON código 1.
SELECT apenom 
FROM alumnos 
WHERE dni = (
	SELECT dni 
	FROM NOTAS 
	WHERE cod = 1);
	
--16 - Obtén el nombre y apellido de los alumnos que no tengan NOTa en la ASignatura cON código 1.
SELECT apenom 
FROM alumnos 
WHERE dni != (
	SELECT dni 
	FROM NOTAS 
	WHERE cod = 1);

--tema 5
--01 - Partiendo de la tabla EMPLE, vISualiza por cada oficio de los empleados del departamento 'VENTAS' la SUMa de salarios.	
SELECT SUM(salario) 
FROM emple NATURAL JOIN depart 
WHERE dnombre = 'VENTAS' 
GROUP BY oficio
--02 - SelecciONa aquellos apellidos de la tabla EMPLE cuyo salario sea igual a la media del salario en su departamento.
SELECT apellido 
FROM emple NATURAL JOIN (
	SELECT AVG(salario) AS media, 
	dept_no FROM emple 
	GROUP BY dept_no 
	ORDER BY dept_no ASC
	) 
WHERE salario = media 
ORDER BY dept_no;

--03 - A partir de la tabla EMPLE, vISualiza el número de empleados de cada departamento cuyo oficio sea 'EMPLEADO'.
SELECT COUNT(emp_no) AS numero_empleados, 
dept_no 
FROM emple 
WHERE oficio = 'EMPLEADO' 
GROUP BY dept_no;

--04 - Desde la tabla EMPLE, vISualiza el departamento que tenga más empleados cuyo oficio sea 'EMPLEADO'.
SELECT dept_no FROM emple 
WHERE oficio = 'EMPLEADO' 
GROUP BY dept_no 
HAVING COUNT(apellido) = (
	SELECT MAX(COUNT(apellido)) AS cuenta 
	FROM emple 
	WHERE oficio = 'EMPLEADO' 
	GROUP BY dept_no
	);
	
--05 - A partir de lAS tablAS EMPLE y DEPART, vISualiza el número de departamento y el nombre de departamento que tenga más empleados cuyo oficio sea 'EMPLEADO'.
SELECT dept_no, dnombre 
FROM depart 
NATURAL JOIN emple 
WHERE oficio = 'EMPLEADO' 
GROUP BY dept_no 
HAVING COUNT(apellido) = (
	SELECT MAX(COUNT(apellido)) AS cuenta 
	FROM emple 
	WHERE oficio = 'EMPLEADO' 
	GROUP BY dept_no
	);
--06 - Busca los departamentos que tienen más de dos personas trabajando en la misma profesión.	
SELECT dept_no 
FROM emple 
NATURAL JOIN depart 
GROUP BY dept_no 
HAVING count(emp_no)>2;

--07 - VISualiza los nombres de los alumnos de la tabla ALUM que aparezcan en estas dos tablas: ANTIGUOS y NUEVOS.
SELECT al.* 
FROM alum al 
LEFT JOIN antiguos an 
ON al.nombre = an.nombre 
LEFT JOIN nuevos nu ON nu.nombre = al.nombre
WHERE an.nombre IS NOT NULL AND nu.nombre IS NOT NULL;

--08 - Escribe las distintas formas en que se puede pONer la cONsulta anterior llegANDo al mISmo resultado.
SELECT al.* 
FROM alum al 
INNER JOIN antiguos an ON al.nombre=an.nombre
INNER JOIN nuevos nu ON nu.nombre = al.nombre;

SELECT * 
FROM alum NATURAL JOIN antiguos
NATURAL JOIN nuevos;

SELECT al.* 
FROM alum al, 
nuevos nu, 
antiguos an 
WHERE al.nombre = an.nombre 
AND al.nombre = nu.nombre;

--09 - VISualiza aquellos nombres de la tabla ALUM que no estén en la tabla ANTIGUOS ni en la tabla NUEVOS.
SELECT al.* 
FROM alum al 
LEFT JOIN nuevos nu ON al.nombre = nu.nombre 
LEFT JOIN antiguos an ON al.nombre = an.nombre
WHERE nu.nombre IS NULL AND an.nombre IS NULL;

--TablAS PERSONAL, PROFESORES Y CENTROS (hacer DESC de lAS tablAS)
--10 - Realiza una cONsulta en la que aparezca por cada centro y en cada especialidad el número de profesores. Si el centro no tiene profesores, debe aparecer un 0 en la columna de número de profesores. LAS columnAS a vISualizar sON: nombre de centro, especialidad y número de profesores.
SELECT c.nombre, CASE WHEN p.especialidad IS NULL THEN 'NINGUNA' ELSE p.especialidad END, CASE WHEN p.cod_centro IS NULL THEN 0 ELSE COUNT(p.cod_centro) END
FROM centros c 
LEFT JOIN profesores p ON p.cod_centro = c.cod_centro
GROUP BY c.nombre, p.especialidad, p.cod_centro;

--11 - Obtén por cada centro el número de empleados. Si el centro carece de empleados, ha de aparecer un 0 como número de empleados.
SELECT 
CASE WHEN p.cod_centro IS NULL THEN c.cod_centro ELSE p.cod_centro END, 
CASE WHEN p.cod_centro IS NULL THEN 0 ELSE COUNT(p.cod_centro) END 
FROM centros c LEFT JOIN persONal p ON p.cod_centro = c.cod_centro
GROUP BY p.cod_centro, c.cod_centro
ORDER BY c.cod_centro ASC;

--12 - Obtén la especialidad con menos profesores.
SELECT 
especialidad, COUNT(especialidad) 
FROM profesores 
GROUP BY especialidad
HAVING COUNT(especialidad) = MIN(COUNT(especialidad));

--TablAS BANCOS, SUCURSALES, CUENTAS y MOVIMIENTOS (hacer DESC de lAS tablAS)
--TABLA BANCOS: CONtiene los datos de los bancos, una fila por cada banco. Un banco se identifica por el COD_BANCO.
--TABLA SUCURSALES: CONtiene los datos de lAS sucursales. Una fila por sucursal. Cada sucursal se identifica por el COD_BANCO+COD_SUCUR.
--TABLA CUENTAS: CONtiene los datos de lAS cuentAS abiertAS en lAS sucursales de los bancos. Una cuenta se identifica por lAS columnAS COD_BANCO+COD_SUCUR+NUM_CTA.
--CONtiene los saldos de lAS cuentAS. SALDO_DEBE cONtiene la SUMa de ReINtegros y SALDO_HABER la SUMa de INgresos.
--TABLA MOVIMIENTOS: CONtiene los movimientos de lAS cuentAS. Una fila representa un movimiento de una cuenta.
--La columna TIPO_MOV puede ser I (INgreso) o R (reINtegro).

--13 - Obtén el banco cON más sucursales. Los datos a obtener sON:
SELECT COUNT(cod_banco) 
FROM sucursales 
GROUP BY cod_banco
HAVING COUNT(cod_banco) = MAX(COUNT(cod_banco));

--14 - El saldo actual de los bancos de 'GUADALAJARA', 1 fila por cada banco:
SELECT nombre_banc AS "Nombre Banco", 
saldo_debe AS "Saldo Debe", 
saldo_haber AS "Saldo Haber" 
FROM bancos NATURAL JOIN sucursales NATURAL JOIN cuentAS 
WHERE poblaciON = 'GUADALAJARA' 
GROUP BY nombre_banc, saldo_debe, saldo_haber;

--15 - Datos de la cuenta o cuentAS cON más movimientos:
SELECT nombre_cta AS "Nombre Cta", COUNT(nombre_cta) AS "Nºmovimientos"
FROM cuentAS NATURAL JOIN movimientos 
GROUP BY nombre_cta
HAVING COUNT(num_cta) = MAX(COUNT(num_cta))

--16 - El nombre de la sucursal que haya tenido más SUMa de reINtegros:
SELECT nombre_suc AS "Nombre sucursal", SUM(importe) AS "SUMa ReINtegros" 
FROM sucursales NATURAL JOIN movimientos 
WHERE tipo_mov = 'R'
GROUP BY nombre_suc
HAVING SUM(importe) = MAX(SUM(importe));

--Tema 6
SELECT * FROM alum NATURAL JOIN nuevos NATURAL JOIN antiguos;
SELECT c.cod_centro 
FROM profesores p 
INNER JOIN centros c ON c.cod_centro = p.cod_centro 
INNER JOIN persONal pe ON pe.cod_centro = c.cod_centro 
GROUP BY pe.funciON, c.cod_centro 
HAVING pe.funciON = 'ADMINISTRATIVO' 
AND COUNT(pe.cod_centro) = 1 
ORDER BY c.cod_centro ASC

INSERT INTO centros (
SELECT c.cod_centro 
FROM profesores p 
INNER JOIN centros c ON c.cod_centro = p.cod_centro 
INNER JOIN persONal pe ON pe.cod_centro = c.cod_centro 
GROUP BY pe.funciON, c.cod_centro 
HAVING pe.funciON = 'ADMINISTRATIVO' 
AND count(pe.cod_centro) = 1 
ORDER BY c.cod_centro ASC), 8790055, 'SalAS, Clara', 'IDIOMA'

--Tema 8
--01 - Construye un bloque PL/SQL que escriba el texto 'Hola'.
SET serveroutput ON;
BEGIN
DBMS_OUTPUT.put_line ('hola');
END;
/
--02 - ¿Qué hace el siguiente bloque PL/SQL?
DECLARE
v_num NUMBER;
BEGIN
SELECT count(*) INTO v_num
FROM productos08;
DBMS_OUTPUT.PUT_LINE(v_num);
END;
/
--imprime 8

--03 - Introduce el bloque anterior desde SQL*Plus y guardarlo en un fichero.
save pruebas.sql;
--04 - Ejecuta la orden SELECT especificada en el bloque anterior desde SQL*Plus sin la cláusula INTO.
SELECT count(*) FROM productos08
--05 - 
DECLARE
v_nom CLIENTES.NOMBRE%TYPE;
BEGIN
SELECT nombre INTO v_nom
FROM clientes
WHERE CLIENTE_NO=&vn_cli;
DBMS_OUTPUT.PUT_LINE(v_nom);
END;
/
--ejemplo funcion
CREATE OR REPLACE FUNCTION insert_incidence2(id INTEGER, descripcion VARCHAR)
RETURNS VOID
AS $$
BEGIN
insert into parte (emp_crea, resuelto, inf_part, tec_res)
values (id, false, descripcion, NULL);
END;
$$
LANGUAGE 'plpgsql';

--syntax oracle
create or replace procedure sumar(
num1 NUMBER, 
num2 NUMBER
) AS 
suma NUMBER(5); 
BEGIN suma := num1 + num2; 
DBMS_OUTPUT.PUT_LINE(suma); 
END sumar;
/
CREATE OR REPLACE FUNCTION nombre_cuenta(
numero_cta NUMBER
)
RETURN VARCHAR2
AS
nombre cuentas.nombre_cta%TYPE;
BEGIN
SELECT nombre_cta INTO nombre FROM cuentas
WHERE num_cta = numero_cta;
DBMS_OUTPUT.PUT_LINE(nombre);
RETURN nombre;
END nombre_cuenta;
/
--Tema 9
--01 - Escribe un procedimiento que reciba dos números y visualice su suma.
create or replace procedure sumar(
num1 NUMBER, 
num2 NUMBER
) AS 
suma NUMBER(5); 
BEGIN suma := num1 + num2; 
DBMS_OUTPUT.PUT_LINE(suma); 
END sumar;
/
--02 - Codifica un procedimiento que reciba una cadena y la visualice al revés.
create or replace procedure reves(
cadena VARCHAR2
)
AS
cadena_reves VARCHAR2(50);
BEGIN
SELECT reverse(cadena) INTO cadena_reves FROM DUAL;
DBMS_OUTPUT.PUT_LINE(cadena_reves);
END reves;
/

--03 - Reescribe el código de los dos ejercicios anteriores para convertirlos en funciones que retornen los valores que mostraban los procedimientos.
create or replace function sumar(
num1 NUMBER, 
num2 NUMBER
)
RETURN NUMBER
AS 
suma NUMBER(5); 
BEGIN
suma := num1 + num2; 
DBMS_OUTPUT.PUT_LINE(suma);
RETURN suma;
END sumar;
/
create or replace FUNCTION reves(
cadena VARCHAR2
)
RETURN VARCHAR2
AS
cadena_reves VARCHAR2(50);
BEGIN
SELECT reverse(cadena) INTO cadena_reves FROM DUAL;
DBMS_OUTPUT.PUT_LINE(cadena_reves);
RETURN cadena_reves;
END reves;
/
--04 - Escribe una función que reciba una fecha y devuelva el año, en número, correspondiente a esa fecha.
CREATE OR REPLACE FUNCTION anio(fecha DATE)
RETURN NUMBER
AS
anio_fecha NUMBER(4);
BEGIN
anio_fecha := EXTRACT(YEAR FROM fecha);
RETURN anio_fecha;
END anio;
/
--05 - Escribe un bloque PL/SQL que haga uso de la función anterior.
CREATE OR REPLACE PROCEDURE use_year(anio NUMBER)
AS
annio NUMBER(4);
BEGIN
annio := anio;
DBMS_OUTPUT.PUT_LINE(annio);
END use_year;
/
--06 - Desarrolla una función que devuelva el número de años completos que hay entre dos fechas que se pasan como parámetros.
CREATE OR REPLACE FUNCTION intervalo(f_inicio DATE, f_fin DATE)
RETURN NUMBER
AS
anios NUMBER(3);
BEGIN
anios := (EXTRACT(YEAR FROM f_fin) - EXTRACT(YEAR FROM f_inicio));
RETURN anios;
END intervalo;
/

--07 - Escribe una función que, haciendo uso de la función anterior, devuelva los trienios que hay entre dos fechas (un trienio son tres años).
CREATE OR REPLACE FUNCTION trienio(anios NUMBER)
RETURN NUMBER
AS
trienios NUMBER(3);
BEGIN
SELECT TRUNC(anios) INTO trienios FROM DUAL;
END trienio;
/

--08 - Codifica un procedimiento que reciba una lista de hasta cinco números y visualice su suma.
CREATE OR REPLACE PROCEDURE suma(val1 NUMBER default 0, val2 NUMBER default 0, val3 NUMBER default 0, val4 NUMBER default 0, val5 NUMBER default 0)
AS
sumando NUMBER(15);
BEGIN
sumando := val1+val2+val3+val4+val5;
DBMS_OUTPUT.PUT_LINE(sumando);
END suma;
/

--09 - Escribe una función que devuelva solamente caracteres alfabéticos sustituyendo cualquier otro carácter por blancos a partir de una cadena que se pasará en la llamada.
CREATE OR REPLACE FUNCTION alphanum(cadena VARCHAR2)
RETURN VARCHAR2
AS
cadena_alpha VARCHAR2(200);
caracter varchar(200);
cod_ascii number(5);
BEGIN
FOR i IN 1..LENGTH(cadena) LOOP
cod_ascii := ASCII(substr(cadena,i,1));
IF (cod_ascii NOT BETWEEN 97 AND 122) AND (cod_ascii NOT BETWEEN 65 AND 90) THEN caracter := ' '; ELSE caracter := substr(cadena,i,1);
END IF;
cadena_alpha := cadena_alpha || caracter;
END LOOP;
RETURN cadena_alpha;
END alphanum;
/
CREATE OR REPLACE PROCEDURE alpha(cadena VARCHAR2)
AS
var VARCHAR(200);
BEGIN
var := cadena;
DBMS_OUTPUT.PUT_LINE(var);
END alpha;
/

--10 - Codifica un procedimiento que permita borrar un empleado cuyo número se pasará en la llamada.
CREATE OR REPLACE PROCEDURE borrar_empleado(id NUMBER)
AS
BEGIN
DELETE FROM emple WHERE EMP_NO = id;
END borrar_empleado;
/
--11 - Escribe un procedimiento que modifique la localidad de un departamento. El procedimiento recibirá como parámetros el número del departamento y la nueva localidad.
CREATE OR REPLACE PROCEDURE actualizar_localidad(dep_num NUMBER, nueva_loc VARCHAR2)
AS
BEGIN
UPDATE depart SET LOC = nueva_loc WHERE DEPT_NO = dep_num;
END actualizar_localidad;
/
--12 - Visualiza todos los procedimientos y funciones del usuario almacenados en la base de datos y su situación (valid o invalid).
SELECT OBJECT_NAME, STATUS FROM USER_OBJECTS;

--Tema 10
--01 - Desarrolla un procedimiento que visualice el apellido y la fecha de alta de todos los empleados ordenados por apellido.
DECLARE
CURSOR c_emple IS
SELECT apellido, fecha_alt FROM emple
ORDER BY apellido;
BEGIN
FOR v_reg_emp IN c_emple LOOP
DBMS_OUTPUT.PUT_LINE(v_reg_emp.apellido||'*'||
v_reg_emp.fecha_alt);
END LOOP;
END;
/
--02 - Codifica un procedimiento que muestre el nombre de cada departamento y el número de empleados que tiene.
CREATE OR REPLACE PROCEDURE showdata
AS
CURSOR c_depart IS
select dnombre, count(dnombre) cuenta from depart natural join emple group by dnombre;
dato1 VARCHAR2(50);
dato2 NUMBER;
BEGIN
FOR v_reg_emp IN c_depart LOOP
DBMS_OUTPUT.PUT_LINE(v_reg_emp.dnombre||'*'||
v_reg_emp.cuenta);
END LOOP;
END showdata;
/
set linesize 10000
set pagesize 500

--03 - Escribe un programa que visualice el apellido y el salario de los cinco empleados que tienen el salario más alto.
--WHILE
CREATE OR REPLACE PROCEDURE showdata2
AS
CURSOR c_emple IS
select apellido, salario from emple order by salario desc;
dato1 VARCHAR2(50);
dato2 NUMBER;
BEGIN
OPEN c_emple;
FETCH c_emple INTO dato1, dato2;
while c_emple%found loop
if c_emple%rowcount <=5 then
DBMS_OUTPUT.PUT_LINE(dato1 || ' ' || dato2);
end if;
FETCH c_emple INTO dato1, dato2;
END LOOP;
CLOSE c_emple;
END showdata2;
/
--FOR
CREATE OR REPLACE PROCEDURE showdata2
AS
CURSOR c_emple IS
select apellido, salario from emple order by salario desc;
dato1 VARCHAR2(50);
dato2 NUMBER;
BEGIN
FOR v_reg_emp IN c_emple LOOP
if c_emple%rowcount <=5 then
DBMS_OUTPUT.PUT_LINE(v_reg_emp.apellido||' '||
v_reg_emp.salario);
end if;
END LOOP;
END showdata2;
/
--04 - Codifica un programa que visualice los dos empleados que ganan menos de cada oficio.
