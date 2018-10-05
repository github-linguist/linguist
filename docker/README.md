# Dockerfile
 Dockefile is created on top of `ubuntu` image.
 # Requirements
 - The project should be cloned from https://github.com/trilogy-group/linguist
 - Docker version 18.06.0-ce
 - Docker compose version 1.22.0
  
# Quick Start
- Clone the repository
- Open a terminal session to that folder
- Run `docker/docker-cli start`
- Run `docker/docker-cli exec`
- At this point you must be inside the docker container, in the root folder of the project. From there, you can run the commands as usual:
	- `script/bootstrap` to prepare the environment
	- `bundle exec rake samples` to download samples
	- `bundle exec bin/github-linguist --breakdown` to run linguist
	- `bundle exec rake test` to run tests.
	
- When you finish working with the container, type `exit`
- Run `docker/docker-cli stop` to stop and remove the service.
 
 Please refer to [Contributing](../CONTRIBUTING.md) doc for more details on the building and running the app.
