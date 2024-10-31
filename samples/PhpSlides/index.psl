<!DOCTYPE html>
<html>
<head>
  <title>Hello World</title>
  <link rel="stylesheet" href="{{ asset('::Styles::Index.css' }}" />
</head>
<body>
  <? /* INCLUDE FILES DIRECTLY */ ?>
  <include path="../component.psl" !/>
  
  <img src="{{ import('../assets/icon.png') }}" />
   
   <ul>
      <? foreach($foo as $bar): ?>
      <li>{{ $bar }}</li>
      <? endforeach ?>
  </ul>
  
   <ul>
      <? while($i < 5): ?>
      <li>Number {{$i}}</li>
      <? endwhile ?>
  </ul>
  
   <ul>
      <? for($i = 0; $i < 5; $i++): ?>
      <li>Number {{$i}}</li>
      <? endfor ?>
  </ul>
  
   <div>
      <? if($is_logged_in): ?>
      <span>Hello {{ $username ? $username : $user }}</span>
      <span>Hello {{ $username && $username }}</span>
      <span>Hello {{ $username ?: $username }}</span>
      <span>Hello {{ $username ?? $username }}</span>
      <? else: ?>
      <span>Hello Guest!</span>
      <? endif ?>
  </div>
</body>
</html>