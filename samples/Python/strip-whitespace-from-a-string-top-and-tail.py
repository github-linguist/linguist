>>> s = ' \t \r \n String with spaces  \t  \r  \n  '
>>> s
' \t \r \n String with spaces  \t  \r  \n  '
>>> s.lstrip()
'String with spaces  \t  \r  \n  '
>>> s.rstrip()
' \t \r \n String with spaces'
>>> s.strip()
'String with spaces'
>>>
