UNAME := $(shell uname)
ifeq ($(UNAME), Windows_NT)
	WHICH := where.exe
	PYTHON := py
	PYTHONINSTALL   := echo Install python3:         https://www.python.org/
	PIPINSTALL      := echo Enable pip:              py -m ensurepip
	GRAPHVIZINSTALL := echo Install GraphViz tools:  https://graphviz.org/
	FFMPEGINSTALL   := echo Install FFmpeg:          https://ffmpeg.org/
	MODULESINSTALL  := echo Install python modules:  py -m pip install pyyaml jsonlib-python3 pygraphviz networkx matplotlib moviepy
	MANIMINSTALL    := echo Install Manim:           py -m pip install manim && python3 -m pip install decorator --upgrade
	CLEAN := rmdir /s /q
else ifeq ($(UNAME), Linux)
	WHICH := which
	PYTHON := python3
	PYTHONINSTALL := apt update && apt install -y python3
	PIPINSTALL := python3 -m ensurepip
	GRAPHVIZINSTALL := apt update && apt install -y graphviz graphviz-dev libgraphviz-dev libpango1.0-dev pkg-config python3-dev xdg-utils
	FFMPEGINSTALL := apt update && apt install -y ffmpeg
	MODULESINSTALL := python3 -m pip install pyyaml jsonlib-python3 pygraphviz networkx matplotlib moviepy
	MANIMINSTALL := python3 -m pip install manim && python3 -m pip install --upgrade moviepy
	CLEAN := rm -rf
else
	WHICH := which
	PYTHON := python3
	PYTHONINSTALL := brew install -q python@3.11
	PIPINSTALL := python3 -m ensurepip
	GRAPHVIZINSTALL := brew install -q graphviz pkg-config
	FFMPEGINSTALL := brew install -q ffmpeg
	MODULESINSTALL := python3 -m pip install pyyaml pygraphviz networkx matplotlib moviepy
	MANIMINSTALL := python3 -m pip install manim && python3 -m pip install --upgrade moviepy
	CLEAN := rm -rf
endif


.PHONY: install run ghs shortestpath clean install-python install-pip install-graphviz install-ffmpeg install-modules install-manim


install: install-manim    
	@printf "\nIt should now be possible to run the code with any of the following commands:\n";
	@printf "    make run\n";
	@printf "    make ghs\n";
	@printf "    make shortest_path\n";
	@printf "    $(PYTHON) src/main.py\n";
	@printf "    $(PYTHON) src/main.py --help\n";

run: 
	@$(PYTHON) src/main.py $(filter-out $@,$(MAKECMDGOALS))

ghs: 
	@$(PYTHON) src/main.py --config ghs/config.yaml --rules ghs/rules.yaml --events ghs/events.json $(filter-out $@,$(MAKECMDGOALS))

shortestpath: 
	@$(PYTHON) src/main.py --config shortestpath/config.yaml --rules shortestpath/rules.yaml --events shortestpath/events.yaml $(filter-out $@,$(MAKECMDGOALS))
	@$(PYTHON) src/main.py --transparency 1 --config shortestpath/config.yaml --rules shortestpath/rules.yaml --events shortestpath/events.yaml $(filter-out $@,$(MAKECMDGOALS))

clean:
	@$(CLEAN) __media__ __slides__ __tmp__ src/__pycache__

install-python:
	@printf "About to install Python 3 by executing\n";
	@printf "    $(PYTHONINSTALL)\n";
	@printf "(note that Manim might not be compatible with the latest version of Python, so it is recommended to install Python version 3.11 or earlier).\n\n";
	@$(PYTHONINSTALL);
	@printf "\n";

install-pip: install-python
	@printf "About to install pip module by executing\n";
	@printf "    $(PIPINSTALL)\n\n";
	@$(PIPINSTALL);
	@printf "\n";

install-graphviz:
	@printf "About to install Graphviz by executing\n";
	@printf "    $(GRAPHVIZINSTALL)\n\n";
	@$(GRAPHVIZINSTALL);
	@printf "\n";

install-ffmpeg:
	@printf "About to install FFmpeg library by executing\n";
	@printf "    $(FFMPEGINSTALL)\n\n";
	@$(FFMPEGINSTALL);
	@printf "\n";

install-modules: install-pip install-graphviz
	@printf "About to install Python modules Yaml, Json, PyGraphViz, NetworkX, MatPlotLib, MoviePy by executing\n";
	@printf "    $(MODULESINSTALL)\n\n";
	@$(MODULESINSTALL);
	@printf "\n";

install-manim: install-modules install-ffmpeg
	@printf "About to install Python module Manim by executing\n";
	@printf "    $(MANIMINSTALL)\n\n";
	@$(MANIMINSTALL);
	@printf "\n";
