# Sample OpenTofu file (.tofu) - This is a hypothetical example for use by
# Linguist to detect HCL in use by OpenTofu with the .tofu extension.

# --------------------------------------------------------------------------
# OpenTofu configuration block
# --------------------------------------------------------------------------
terraform {
  required_version = ">= 1.0.0"
  required_providers {
    # This is just an example comment
    aws    = {
      source  = "hashicorp/aws"
      version = "~> 3.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.0"
    }
  }

  # Backend configuration
  backend "s3" {
    bucket = "my-tofu-backend-bucket"
    key    = "path/to/statefile.tfstate"
    region = "us-west-2"
  }
}

# --------------------------------------------------------------------------
# Provider configurations
# --------------------------------------------------------------------------
provider "aws" {
  region = var.aws_region
}

provider "random" {
  # random provider typically doesn't require complicated config
}

# --------------------------------------------------------------------------
# Variable definitions
# --------------------------------------------------------------------------
variable "aws_region" {
  type        = string
  description = "AWS region to deploy resources into."
  default     = "us-west-2"
}

variable "instance_count" {
  type        = number
  description = "Number of instances to launch."
  default     = 2
}

variable "tags_map" {
  type        = map(string)
  description = "Map of tags to apply to resources."
  default     = {
    Environment = "Development"
    Application = "DemoApp"
  }
}

# --------------------------------------------------------------------------
# Local values
# --------------------------------------------------------------------------
locals {
  full_tag_map = merge(
    var.tags_map,
    {
      CreatedBy  = "OpenTofuSample"
      Timestamp  = formatdate("YYYY-MM-DD", timestamp())
    }
  )
}

# --------------------------------------------------------------------------
# Dummy resource blocks
# --------------------------------------------------------------------------
resource "aws_instance" "example" {
  # Demo AMI, not real
  ami           = "ami-00000000000000000"
  instance_type = "t2.micro"

  count = var.instance_count

  tags = merge(
    local.full_tag_map,
    { Name = "ExampleInstance-${count.index}" }
  )
}

resource "random_pet" "pet_name" {
  length = 3
}

resource "aws_s3_bucket" "example_bucket" {
  bucket = "my-example-bucket-${random_pet.pet_name.id}"
  acl    = "private"

  tags = local.full_tag_map
}

# --------------------------------------------------------------------------
# Module blocks
# --------------------------------------------------------------------------
module "fake_module" {
  source  = "./modules/fake_module"  # local path or remote module
  version = "0.0.1"                  # dummy version

  # Pass in some variables
  example_var       = "HelloTofu"
  another_var       = var.instance_count
  inherited_tag_map = local.full_tag_map
}

# --------------------------------------------------------------------------
# Outputs
# --------------------------------------------------------------------------
output "instance_ids" {
  description = "List of created instance IDs."
  value       = aws_instance.example[*].id
}

output "random_pet_name" {
  description = "The randomly generated pet name."
  value       = random_pet.pet_name.id
}

output "example_bucket_name" {
  description = "The created S3 bucket name."
  value       = aws_s3_bucket.example_bucket.bucket
}
