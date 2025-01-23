<?hh // strict

require_once $_SERVER['DOCUMENT_ROOT'].'/core/funs/init.php';

final class MySecureRequest {
  public function __construct(private Map<string, mixed> $GETParams) {}
  public function stringParam(string $name): UNESCAPED_STRING {
    invariant($this->GETParams->contains($name), 'Unknown GET param: '.$name);
    $raw_string = $this->GETParams[$name];
    invariant(is_string($raw_string), $name.' is not a string');
    return unescaped_string($raw_string);
  }
}
