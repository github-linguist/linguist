<?php

class Vector
{
	private $values;

	public function setValues(array $values)
	{
		if (count($values) != 3)
			throw new Exception('Values must contain exactly 3 values');
		foreach ($values as $value)
			if (!is_int($value) && !is_float($value))
				throw new Exception('Value "' . $value . '" has an invalid type');
		$this->values = $values;
	}

	public function getValues()
	{
		if ($this->values == null)
			$this->setValues(array (
					0,
					0,
					0
			));
		return $this->values;
	}

	public function Vector(array $values)
	{
		$this->setValues($values);
	}

	public static function dotProduct(Vector $va, Vector $vb)
	{
		$a = $va->getValues();
		$b = $vb->getValues();
		return ($a[0] * $b[0]) + ($a[1] * $b[1]) + ($a[2] * $b[2]);
	}

	public static function crossProduct(Vector $va, Vector $vb)
	{
		$a = $va->getValues();
		$b = $vb->getValues();
		return new Vector(array (
				($a[1] * $b[2]) - ($a[2] * $b[1]),
				($a[2] * $b[0]) - ($a[0] * $b[2]),
				($a[0] * $b[1]) - ($a[1] * $b[0])
		));
	}

	public static function scalarTripleProduct(Vector $va, Vector $vb, Vector $vc)
	{
		return self::dotProduct($va, self::crossProduct($vb, $vc));
	}

	public static function vectorTrippleProduct(Vector $va, Vector $vb, Vector $vc)
	{
		return self::crossProduct($va, self::crossProduct($vb, $vc));
	}
}

class Program
{

	public function Program()
	{
		$a = array (
				3,
				4,
				5
		);
		$b = array (
				4,
				3,
				5
		);
		$c = array (
				-5,
				-12,
				-13
		);
		$va = new Vector($a);
		$vb = new Vector($b);
		$vc = new Vector($c);

		$result1 = Vector::dotProduct($va, $vb);
		$result2 = Vector::crossProduct($va, $vb)->getValues();
		$result3 = Vector::scalarTripleProduct($va, $vb, $vc);
		$result4 = Vector::vectorTrippleProduct($va, $vb, $vc)->getValues();

		printf("\n");
		printf("A = (%0.2f, %0.2f, %0.2f)\n", $a[0], $a[1], $a[2]);
		printf("B = (%0.2f, %0.2f, %0.2f)\n", $b[0], $b[1], $b[2]);
		printf("C = (%0.2f, %0.2f, %0.2f)\n", $c[0], $c[1], $c[2]);
		printf("\n");
		printf("A · B = %0.2f\n", $result1);
		printf("A × B = (%0.2f, %0.2f, %0.2f)\n", $result2[0], $result2[1], $result2[2]);
		printf("A · (B × C) = %0.2f\n", $result3);
		printf("A × (B × C) =(%0.2f, %0.2f, %0.2f)\n", $result4[0], $result4[1], $result4[2]);
	}
}

new Program();
?>
