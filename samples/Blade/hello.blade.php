<!DOCTYPE html>
<html>
<head>
    <title>@yield('title', 'We love GitHub')</title>
    @stack('scripts')
    @stack('styles')
</head>
<body>
    @include('partials.nav')

    @yield('content')

    <ul>
        @foreach($foo as $bar)
        <li>{{ $bar }}</li>
        @endforeach
    </ul>
</body>
</html>
