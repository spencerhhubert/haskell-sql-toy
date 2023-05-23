import requests

query = "SELECT * WHERE height > 2;"
url = "http://localhost:4000"
path = "/sql/"

res = requests.get(url+path, params={"query": query})
print(res.text)
