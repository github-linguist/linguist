<!DOCTYPE html>
<html>
<head>
    <title>@yield('pageTitle', 'My Default Title')</title>
</head>
<body>
    @section('content')

    <ul>
        @foreach($key as $foo)
        <li>Foo bar baz {{ $foo }}</li>
        @endforeach
    </ul>
</body>
</html>
