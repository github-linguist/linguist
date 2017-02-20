resource "aws_security_group" "elb_sec_group" {
  description = "Allow traffic from the internet to ELB port 80"
  vpc_id = "${var.vpc_id}"

  ingress {
      from_port = 80
      to_port = 80
      protocol = "tcp"
      cidr_blocks = ["${split(",", var.allowed_cidr_blocks)}"]
  }

  egress {
      from_port = 0
      to_port = 0
      protocol = "-1"
      cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "dokku_allow_ssh_from_internal" {
  description = "Allow git access over ssh from the private subnet"
  vpc_id = "${var.vpc_id}"

  ingress {
      from_port = 22
      to_port = 22
      protocol = "tcp"
      cidr_blocks = ["${var.private_subnet_cidr}"]
  }

  egress {
      from_port = 0
      to_port = 0
      protocol = "-1"
      cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "allow_from_elb_to_instance" {
  description = "Allow traffic from the ELB to the private instance"
  vpc_id = "${var.vpc_id}"

  ingress {
      security_groups = ["${aws_security_group.elb_sec_group.id}"]
      from_port = 80
      to_port = 80
      protocol = "tcp"
  }

  egress {
      from_port = 0
      to_port = 0
      protocol = "-1"
      cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_instance" "dokku" {
  ami = "ami-47a23a30"
  instance_type = "${var.instance_type}"
  associate_public_ip_address = false
  key_name = "${var.key_name}"
  subnet_id = "${var.private_subnet_id}"
  vpc_security_group_ids = [
    "${var.bastion_sec_group_id}",
    "${aws_security_group.allow_from_elb_to_instance.id}",
    "${aws_security_group.dokku_allow_ssh_from_internal.id}"
  ]
  tags {
    Name = "${var.name}"
  }
  connection {
    user = "ubuntu"
    private_key = "${var.private_key}"
    bastion_host = "${var.bastion_host}"
    bastion_port = "${var.bastion_port}"
    bastion_user = "${var.bastion_user}"
    bastion_private_key = "${var.bastion_private_key}"
  }
  provisioner "file" {
    source = "${path.module}/../scripts/install-dokku.sh"
    destination = "/home/ubuntu/install-dokku.sh"
  }
  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/ubuntu/install-dokku.sh",
      "HOSTNAME=${var.hostname} /home/ubuntu/install-dokku.sh"
    ]
  }
}

resource "aws_elb" "elb_dokku" {
  name = "elb-dokku-${var.name}"
  subnets = ["${var.public_subnet_id}"]
  security_groups = ["${aws_security_group.elb_sec_group.id}"]

  listener {
    instance_port = 80
    instance_protocol = "http"
    lb_port = 80
    lb_protocol = "http"
  }

  health_check {
    healthy_threshold = 2
    unhealthy_threshold = 2
    timeout = 3
    target = "HTTP:80/"
    interval = 30
  }

  instances = ["${aws_instance.dokku.id}"]
  cross_zone_load_balancing = false
  idle_timeout = 400

  tags {
    Name = "elb-dokku-${var.name}"
  }
}

resource "aws_route53_record" "dokku-deploy" {
   zone_id = "${var.zone_id}"
   name = "deploy.${var.hostname}"
   type = "A"
   ttl = "300"
   records = ["${aws_instance.dokku.private_ip}"]
}

resource "aws_route53_record" "dokku-wildcard" {
   zone_id = "${var.zone_id}"
   name = "*.${var.hostname}"
   type = "CNAME"
   ttl = "300"
   records = ["${aws_elb.elb_dokku.dns_name}"]
}