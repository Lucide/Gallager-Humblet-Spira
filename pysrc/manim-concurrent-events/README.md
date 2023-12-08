# Manim-based renderer of concurrent events


## Requires: 

	make 

For example, on Ubuntu this can be made available by 

	apt update && apt install -y build-essential


## Install with

	make

This will install additional software, libraries, modules, e.g.:
- python3, python3-pip
- libpango1.0-dev, pkg-config, python3-dev, graphviz, graphviz-dev, libgraphviz-dev, xdg-utils
- python modules pyyaml, jsonlib-python3, pygraphviz, networkx, matplotlib, moviepy, manim
- ffmpeg

This has been only tested on Ubuntu, and will not work on other Unix systems.
On Mac OS it might work, but there are some dependencies issues between pymovie and manim.
On Windows one needs to manually install everything...


## Run with

	make run

or

	python3 src/main.py

Help on command line arguments with

	make run -- --help

or

	python3 src/main.py --help

This will create:
- a video under the folder __media__
- some html slides under the folder __slides__


## Clean up

	make clean

This will erase the folders
- __media__
- __slides__
- __tmp__
- src/__pycache__
- demos/ * .beam


## Files that can be edited

	events.yaml
	config.yaml
	rules.yaml
