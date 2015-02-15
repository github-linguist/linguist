def varname = 'foo'
def value = 42

new GroovyShell(this.binding).evaluate("${varname} = ${value}")

assert foo == 42
