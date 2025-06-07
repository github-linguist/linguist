# A simple configuration example in Nickel
let name = "my-service" in
let port = 8080 in
{
  server = {
    name = name,
    port = port,
    host = "localhost",
    ssl = false,
  },
  database = {
    url = "postgresql://localhost:5432/mydb",
    pool_size = 10,
  },
  features = {
    logging = true,
    metrics = true,
    tracing = false,
  },
}