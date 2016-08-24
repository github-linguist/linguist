import sublime, sublime_plugin
import os, time
import subprocess

from sublime_lib.path import root_at_packages, get_package_name

class ManPagePreview(sublime_plugin.WindowCommand):
    def run(self):
	# exit if file is dirty, we can't run a man command against a file that doesn't exist
        if self.window.active_view().is_dirty():
            o = self.window.get_output_panel("manfail")
            o.run_command("insert_snippet", {"contents": "Unable to preview unsaved file."})
            self.window.run_command("show_panel", {"panel": "output.manfail"})
            return

        # process document with groff
        curpath = self.window.active_view().file_name()
        c = subprocess.Popen(["groff", "-Tascii", "-man", curpath], stdout=subprocess.PIPE)
        output, err = c.communicate()

        # run groff output through col to clean it up
        col = subprocess.Popen(["col", "-bx"], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        cleanout, err = col.communicate(output)

        # write clean output to new window
        v = self.window.new_file()
        v.settings().set('default_dir', root_at_packages('User'))
        v.set_syntax_file('Packages/Man Page Support/man-preview.tmLanguage')
        e = v.begin_edit()
        p = v.text_point(0,0)
        v.insert(e, p, cleanout)
        v.end_edit(e)

class ManPageNewCommand(sublime_plugin.WindowCommand):
    def run(self):
        v = self.window.new_file()
        v.settings().set('default_dir', root_at_packages('User'))
        v.set_syntax_file('Packages/Man Page Support/man-groff.tmLanguage')

        template = """.\\\" Manpage for ${1:<COMMAND>}.
.\\\" Contact ${2:<AUTHOR_EMAIL>} to correct errors or typos.
.TH man 8 "%s" "1.0" "${1:<COMMAND>}"
.SH NAME
${1:<COMMAND>}
.SH SYNOPSIS
.SY
${1:<COMMAND>}
.YS
.SH DESCRIPTION
${1:<COMMAND>}
.SH BUGS
No known bugs.
.SH SEE ALSO
.SH AUTHOR
.MT ${2:<AUTHOR_EMAIL>}
${3:<AUTHOR_NAME>}
.ME
""" %(time.strftime("%B %Y"))
        v.run_command("insert_snippet", {"contents": template})
