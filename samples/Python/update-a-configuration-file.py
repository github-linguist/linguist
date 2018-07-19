#!/usr/bin/env python

#----------------------------------------------------------------------------
# STANDARD MODULES
#----------------------------------------------------------------------------
import re
import string


#----------------------------------------------------------------------------
# GLOBAL: VARIABLES
#----------------------------------------------------------------------------
DISABLED_PREFIX = ';'


#----------------------------------------------------------------------------
# CLASS Option
#----------------------------------------------------------------------------
class Option(object):
    """An option, characterized by its name and its (optional) value. and by
       its status, which can be enabled or disabled.
       If its value is None, it is regarded to as a boolean option with a
       value of true.
    """

    #------------------------------------------------------------------------
    def __init__(self, name, value=None, disabled=False,
                 disabled_prefix=DISABLED_PREFIX):
        """Create an Option instance, setting its name to 'name' (always
           converted to a string) and its value to 'value'. If 'disabled' is
           True, the option is considered disabled, otherwise enabled.
           The string 'disabled_prefix' is used as a prefix when generating the
           string representation of the option.
        """
        self.name = str(name)
        self.value = value
        self.disabled = bool(disabled)
        self.disabled_prefix = disabled_prefix

    #------------------------------------------------------------------------
    def __str__(self):
        """Return a string representation of the Option instance.
           This always includes the option name, followed by a space and the
           option value (if it is not None). If the option is disabled, the
           whole string is preprendend by the string stored in the instance
           attribute 'disabled_prefix' and a space.
        """
        disabled = ('', '%s ' % self.disabled_prefix)[self.disabled]
        value = (' %s' % self.value, '')[self.value is None]
        return ''.join((disabled, self.name, value))

    #------------------------------------------------------------------------
    def get(self):
        """Return the option value.
           If the stored value is None, the option is regarded to as a
           boolean one and its enabled status is returned. Othrwise its value
           is returned.
        """
        enabled = not bool(self.disabled)
        if self.value is None:
            value = enabled
        else:
            value = enabled and self.value
        return value


#----------------------------------------------------------------------------
# CLASS Config
#----------------------------------------------------------------------------
class Config(object):
    """A set of configuration options and comment strings.
    """
    # Regular expression matching a valid option line.
    reOPTION = r'^\s*(?P<disabled>%s*)\s*(?P<name>\w+)(?:\s+(?P<value>.+?))?\s*$'

    #------------------------------------------------------------------------
    def __init__(self, fname=None, disabled_prefix=DISABLED_PREFIX):
        """Initialize a Config instance, optionally reading the contents of
           the configuration file 'fname'.
           The string 'disabled_prefix' is used as a prefix when generating the
           string representation of the options.
        """
        self.disabled_prefix = disabled_prefix
        self.contents = []          # Sequence of strings and Option instances.
        self.options = {}           # Map an option name to an Option instance.
        self.creOPTION = re.compile(self.reOPTION % self.disabled_prefix)
        if fname:
            self.parse_file(fname)

    #------------------------------------------------------------------------
    def __str__(self):
        """Return a string representation of the Config instance.
           This is just the concatenation of all the items stored in the
           attribute 'contents'.
        """
        return '\n'.join(map(str, self.contents))

    #------------------------------------------------------------------------
    def parse_file(self, fname):
        """Parse all the lines of file 'fname' by applying the method
           'parser_lines' on the file contents.
        """
        with open(fname) as f:
            self.parse_lines(f)
        return self

    #------------------------------------------------------------------------
    def parse_lines(self, lines):
        """Parse all the lines of iterable 'lines' by invoking the method
           'parse_line' for each line in 'lines'.
        """
        for line in lines:
            self.parse_line(line)
        return self

    #------------------------------------------------------------------------
    def parse_line(self, line):
        """Parse the line 'line', looking for options.
           If an option line is found, spaces are stripped from the start and
           the end of 'line' and any non-printable character is removed as well.
           Only the first occurrence of an option is processed, all the other
           occurrences are ignored. A valid option is added to the instance
           attribute 'contents' (in order to preserve its position among the
           other lines). It is also added to the mapping stored in the instance
           attribute 'options'.
           Any non-option string is added the the instance attribute 'contents',
           except those lines starting with the string stored into the instance
           attribute 'disabled_prefix' which are not followed by any option
           name.
        """
        s = ''.join(c for c in line.strip() if c in string.printable)
        moOPTION = self.creOPTION.match(s)
        if moOPTION:
            name = moOPTION.group('name').upper()
            if not name in self.options:
                self.add_option(name, moOPTION.group('value'),
                                moOPTION.group('disabled'))
        else:
            if not s.startswith(self.disabled_prefix):
                self.contents.append(line.rstrip())
        return self

    #------------------------------------------------------------------------
    def add_option(self, name, value=None, disabled=False):
        """Create a new Option instance, named 'name' (always converted to
           uppercase) with value 'value' and set its disabled status to
           'disabled'.
           The Option instance is added to the instance attribute 'contents'.
           It is also added to the mapping stored in the instance attribute
           'options'.
        """
        name = name.upper()
        opt = Option(name, value, disabled)
        self.options[name] = opt
        self.contents.append(opt)
        return opt

    #------------------------------------------------------------------------
    def set_option(self, name, value=None, disabled=False):
        """Look for an option named 'name' (always converted to
           uppercase) among the options stored in the instance
           attribute 'options'.
           If it is not found, a new Option instance is created.
           In any case its value is set to 'value' and its disabled
           status to 'disabled'.
        """
        name = name.upper()
        opt = self.options.get(name)
        if opt:
            opt.value = value
            opt.disabled = disabled
        else:
            opt = self.add_option(name, value, disabled)
        return opt

    #------------------------------------------------------------------------
    def enable_option(self, name, value=None):
        """Enable the option named 'name' (always converted to
           uppercase) and set its value to 'value'.
           If the option is not found, it is created and added to the
           end of the instance attribute 'contents'.
        """
        return self.set_option(name, value, False)

    #------------------------------------------------------------------------
    def disable_option(self, name, value=None):
        """Disable the option named 'name' (always converted to
           uppercase) and set its value to 'value'.
           If the option is not found, it is created and added to the
           end of the instance attribute 'contents'.
        """
        return self.set_option(name, value, True)

    #------------------------------------------------------------------------
    def get_option(self, name):
        """Return the value of the option named 'name' (always
           converted to uppercase).
           If the option is not found in the instance attribute
           'options', None is returned. If the stored value is None,
           it is regarded to as a boolean option and its enable status
           is returned. Otherwise its value is returned.
        """
        opt = self.options.get(name.upper())
        value = opt.get() if opt else None
        return value


#----------------------------------------------------------------------------
# MAIN
#----------------------------------------------------------------------------
if __name__ == '__main__':
    import sys
    cfg = Config(sys.argv[1] if len(sys.argv) > 1 else None)
    cfg.disable_option('needspeeling')
    cfg.enable_option('seedsremoved')
    cfg.enable_option('numberofbananas', 1024)
    cfg.enable_option('numberofstrawberries', 62000)
    print cfg
