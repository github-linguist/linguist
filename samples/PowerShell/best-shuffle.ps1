function Best-Shuffle($strings){
	foreach($string in $strings){
		$sa1 = $string.ToCharArray()
		$sa2 = Get-Random -InputObject $sa1 -Count ([int]::MaxValue)
		$string = [String]::Join("",$sa2)
		echo $string
	}
}

Best-Shuffle "abracadabra", "seesaw", "pop", "grrrrrr", "up", "a"
