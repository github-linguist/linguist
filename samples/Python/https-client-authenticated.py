import httplib

connection = httplib.HTTPSConnection('www.example.com',cert_file='myCert.PEM')
connection.request('GET','/index.html')
response = connection.getresponse()
data = response.read()
