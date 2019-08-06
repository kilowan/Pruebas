<script>
//Cabecera
document.write('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" \
    "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"> \
<html xmlns="http://www.w3.org/1999/xhtml"> \
	<head> \
		<title>Main</title> \
		<LINK REL=StyleSheet HREF="formato.css" TYPE="text/css" MEDIA=screen> \
		<meta http-equiv="content-type" content="text/html; charset=utf-8" /> \
	</head> \
	<body> \
			<div class="header"> \
		</div> \
		<div class="cuerpo"> \
		<form action="pagina3.php" method="post" > \
		<input type="hidden" name="funcion" value="busca" /> \
		<table> \
			<tr>');
</script>
<?php
$data = "";
//Funciones
	function check()
	{
		if(isset($_POST['funcion']))
		{
			$funcion = $_POST['funcion'];
		}
		elseif(isset($_GET['funcion']))
		{
			$funcion = $_GET['funcion'];
		}
		return $funcion;
	}
	function conn()
	{
		$host_db = "localhost";
		$user_db = "test";
		$pass_db = "1234";
		$db_name = "almacen";
		return new mysqli($host_db, $user_db, $pass_db, $db_name);
	}
	function buscar()
	{
			return '<td><input name="entrada" type="text" id="entrada" required></td>
			<td>
				<select class="select" name="tipo">
					<option value="serie">Número de serie</option>
					<option value="nombre">Nombre</option>
					<option value="zona">Ubicación</option>
				</select>
			</td>
			<td>
				<input type="submit" name="Submit" class="boton" value="busca" />
			</td>
		</tr>
		<tr>
			<td colspan="3"><a href="main.html"class="boton">Atrás</a></td>
		</tr>';
	}
	function serie($entrada, $conexion)
	{
		$con = $conexion->query("SELECT  
		i.nombre AS 'nombre_producto', 
		i.precio, 
		i.descripcion,
		p.num_serie, 
		p.cantidad, 
		z.nombre
		FROM info_producto i, productos p, zona z
		WHERE i.id_producto = p.id AND i.id_zona = z.id AND p.num_serie = '{$entrada}'");
		return $con;
	}
	function nombre($entrada, $conexion)
	{
		$con = $conexion->query("SELECT  
		i.nombre AS 'nombre_producto', 
		i.precio, 
		i.descripcion,
		p.num_serie, 
		p.cantidad, 
		z.nombre
		FROM info_producto i, productos p, zona z
		WHERE i.id_producto = p.id AND i.id_zona = z.id AND i.nombre = '{$entrada}'");
		return $con;
	}
	function zona($entrada, $conexion)
	{
		$con = $conexion->query("SELECT  
		i.nombre AS 'nombre_producto', 
		i.precio, 
		i.descripcion,
		p.num_serie, 
		p.cantidad, 
		z.nombre
		FROM info_producto i, productos p, zona z
		WHERE i.id_producto = p.id AND i.id_zona = z.id AND z.nombre = '{$entrada}'");
		return $con;
	}
	function busca($conexion, $tipo, $entrada)
	{
		$data = '
		<table>
			<tr>
				<th>Nombre articulo</th>
				<th>Zona</th>
				<th>Precio</th>
				<th>Descripción</th>
				<th>Nº de serie</th>
				<th>Cantidad</th>
			</tr>
			<tr>';
		$con = $tipo($entrada, $conexion);	
		while($fila = mysqli_fetch_assoc($con))
		{
			$data = $data.'
			<td>'.$fila['nombre_producto'].'</td>
			<td>'.$fila['nombre'].'</td>
			<td>'.$fila['precio'].'</td>
			<td>'.$fila['descripcion'].'</td>
			<td>'.$fila['num_serie'].'</td>
			<td>'.$fila['cantidad'].'</td>
			</tr>';
		}
		$data = $data.'
		<tr colspan="6">
			<td colspan="6">
				<a href="pagina3.php?funcion=buscar" class="boton">Atrás</a>
				<a href="main.html" class="boton">Main</a>
			</td>
		</tr>';
		return $data;
	}	
//Cuerpo
$funcion = check();
if($funcion == "buscar")
{
	$data = $data.$funcion();
}
else if($funcion == 'busca')
{
		$conexion = conn();
		$tipo = $_POST['tipo'];
		$entrada = $_POST['entrada'];
		$data = $data.$funcion($conexion, $tipo, $entrada);
}
		$data = $data.'</table>
		</div>
			<div class="Pie">
				<img src="salto.png">
			</div>
	</body>
</html>';
echo $data;
?>