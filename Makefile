project=$(shell basename `pwd`)

all: build run
solve:
	@stack solver --modify-stack-yaml

run:
	@stack exec $(project)

deps:
	@vim $(project).cabal

build:
	@stack build
	@echo -e "\e[1mBuild succesful\e[0m"

help:
	@echo "solve → if changed dependencies"
	@echo "run   → run compiled executabe"
	@echo "deps  → edit .cabal file"
	@echo "build → compile"
