#!/bin/bash

docker run --rm -p 8888:8888 \
  -v $(pwd):/home/jovyan/work \
  --name note-fourier \
  rubydata/datascience-notebook

# docker exec -it note-fourier bash
