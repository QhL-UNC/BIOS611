#!/bin/bash

docker build . --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g) -t project
docker run - $(pwd):/home/rstudio/work -p 8787:8787 -it project