$fac = {
    	param([ScriptBlock] $f)
    	invoke-expression @"
    	{
    		param([int] `$n)
    		if (`$n -le 0) {1}
    		else {`$n * {$f}.InvokeReturnAsIs(`$n - 1)}
    	}
"@
    }

$fib = {
	param([ScriptBlock] $f)
	invoke-expression @"
	{
		param([int] `$n)
		switch (`$n)
        {
        0 {1}
        1 {1}
        default {{$f}.InvokeReturnAsIs(`$n-1)+{$f}.InvokeReturnAsIs(`$n-2)}
        }
	}
"@
}

$Z = {
    param([ScriptBlock] $f)
    invoke-expression @"
    {
        param([ScriptBlock] `$x)
        {$f}.InvokeReturnAsIs(`$(invoke-expression @`"
        {
            param(```$y)
            {`$x}.InvokeReturnAsIs({`$x}).InvokeReturnAsIs(```$y)
        }
`"@))
    }.InvokeReturnAsIs({
        param([ScriptBlock] `$x)
        {$f}.InvokeReturnAsIs(`$(invoke-expression @`"
        {
            param(```$y)
            {`$x}.InvokeReturnAsIs({`$x}).InvokeReturnAsIs(```$y)
        }
`"@))
    })
"@
}

$Z.InvokeReturnAsIs($fac).InvokeReturnAsIs(5)
$Z.InvokeReturnAsIs($fib).InvokeReturnAsIs(5)
