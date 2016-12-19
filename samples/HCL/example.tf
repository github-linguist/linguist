resource "aws_instance" "web" {
  // Copies the myapp.conf file to /etc/myapp.conf
  provisioner "file" {
    source = "conf/myapp.conf"
    destination = "/etc/myapp.conf"
  }

  // Copies the configs.d folder to /etc/configs.d
  provisioner "file" {
    source = "conf/configs.d"
    destination = "/etc"
  }
}
