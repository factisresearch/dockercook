#!/bin/bash
stack exec dockercook init
stack exec -- dockercook cook -i .gitignore -t dockercook_ bootstrap/bin-img.cook
