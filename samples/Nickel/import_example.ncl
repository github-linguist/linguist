# Example showing imports and composition
import "config.ncl" as base_config in

base_config & {
  server.port = 9000,
  environment = "production",
  features.monitoring = true,
  new_feature = {
    enabled = true,
    settings = ["option1", "option2"],
  },
}