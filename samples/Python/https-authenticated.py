#!/usr/bin/python
# -*- coding: utf-8 -*-

from mechanize import Browser

USER_AGENT = "Mozilla/5.0 (X11; U; Linux i686; tr-TR; rv:1.8.1.9) Gecko/20071102 Pardus/2007 Firefox/2.0.0.9"

br = Browser()
br.addheaders = [("User-agent", USER_AGENT)]

# remove comment if you get debug output
# br.set_debug_redirects(True)
# br.set_debug_responses(True)
# br.set_debug_http(True)

br.open("https://www.facebook.com")

br.select_form("loginform")
br['email'] = "xxxxxxx@xxxxx.com"
br['pass'] = "xxxxxxxxx"
br['persistent'] = ["1"]

response = br.submit()
print response.read()
