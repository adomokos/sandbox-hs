$(GHCID_BIN):
	stack install ghcid

build: ## Build with Stack
	stack build
.PHONY: build

test: ## Run the tests
	stack build --fast --test --test-arguments=--format=progress -j4
	# stack build --fast --test
.PHONY: test

repl: ## Run a REPL for development
	stack ghci :sandbox-hs-exe
.PHONY: repl

repl-test: ## Run a REPL with tests
	stack ghci :sandbox-hs-test
.PHONY: repl-test

run: ## Run app locally
	stack exec -- sandbox-hs-exe
.PHONY: run

run-parser: ## Run the parser example
	stack build :command-line-parser-exe --fast --exec "command-line-parser-exe Smith -e"
.PHONY: run-parser

help: ## Prints this help command
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) |\
		sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
