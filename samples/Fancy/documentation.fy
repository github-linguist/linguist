def class Foo {
  """
  This is a docstring. Every object in Fancy can have it's own docstring.
  Either defined in the source code, like this one, or by using the Object#docstring: method
  """
  def a_method {
    """
    Same for methods. They can have docstrings, too.
    """
  }
}
Foo docstring println # prints docstring Foo class
Foo instance_method: 'a_method . docstring println # prints method's docstring
