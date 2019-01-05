package httpapi.authz

# bob is alice's manager, and betty is charlie's.
subordinates = {"alice": [], "charlie": [], "bob": ["alice"], "betty": ["charlie"]}

# HTTP API request
import input as http_api

default allow = false

# Allow users to get their own salaries.
allow {
  http_api.method = "GET"
  http_api.path = ["finance", "salary", username]
  username = http_api.user
}

# Allow managers to get their subordinates' salaries.
allow {
  http_api.method = "GET"
  http_api.path = ["finance", "salary", username]
  subordinates[http_api.user][_] = username
}
