# Terragrunt is a thin wrapper for Terraform that provides extra tools for working with multiple Terraform modules,
# remote state, and locking: https://github.com/gruntwork-io/terragrunt
terragrunt = {
  # Configure Terragrunt to automatically store tfstate files in an S3 bucket
  remote_state {
    backend = "s3"
    config {
      encrypt        = true
      bucket         = "acme-main-terraform-state"
      key            = "${path_relative_to_include()}/terraform.tfstate"
      region         = "us-east-1"
      dynamodb_table = "terraform-locks"
    }
  }

  # Configure Terragrunt to use common var files to help you keep often-repeated variables (e.g., account ID) DRY.
  # Note that even though Terraform automatically pulls in terraform.tfvars, we include it explicitly at the end of the
  # list to make sure its variables override anything in the common var files.
  terraform {
    extra_arguments "common_vars" {
      commands = ["${get_terraform_commands_that_need_vars()}"]

      optional_var_files = [
        "${get_tfvars_dir()}/${find_in_parent_folders("account.tfvars", "skip-account-if-does-not-exist")}",
        "${get_tfvars_dir()}/${find_in_parent_folders("region.tfvars", "skip-region-if-does-not-exist")}",
        "${get_tfvars_dir()}/${find_in_parent_folders("env.tfvars", "skip-env-if-does-not-exist")}",
        "${get_tfvars_dir()}/terraform.tfvars"
      ]
    }
  }
}

key1 = "val1"
key2 = 0
key3 = 1
key4 = true

# Sample comments
key5 = false

key6 = ["hello", "from", "gruntwork.io"]

key7 = {
  key1 = "hello"
  key2 = "from"
  key3 = "gruntwork.io"
}

key8 = [
  {
    keyA = "hello"
    keyB = "there"
  },
  {
    keyA = "hello"
    keyB = "there"
  }
]