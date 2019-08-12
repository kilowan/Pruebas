<?php
const A = '71-9-449-';
function number($n, $r)
{
	if($r !=0)
	{
		for($i = 0; $i<$r; $i++)
		{
			$n--;
			if($n <0)
			{
				$n = 9;
			}
		}
	}
	return $n;
}
function texto($x, $n, $a)
{
	for($i = 0;$i<=4;$i++)
	{
		if($i == 0 || $i == 2)
		{
			$in = '0';
			$number = number($n, 0);
		}
		else if($i == 1)
		{
			$in = '7';
			$number = number($n, 5);
		}
		else if($i >= 3)
		{
			$in = '5';
			$number = number($n, 1);
		}
		if($i == 0 || $i == 4)
		{
			$header = '96';
		}
		else if($i == 1 || $i == 2 || $i == 3)
		{
			$header = '92';
		}
		$v1 = $header.'-'.$a.$in.$x.'-'.$number.'<br />';
		echo $v1;
	}
}
texto('02', 5, A);
?>