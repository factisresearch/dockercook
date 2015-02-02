#!/bin/bash
DOCKERCOOK=dist/build/dockercook/dockercook
"$DOCKERCOOK" init
"$DOCKERCOOK" cook -d . -b bootstrap/ -i .gitignore -t dockercook_ bin-img.cook
