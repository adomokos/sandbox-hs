$(GHCID_BIN):
	stack install ghcid

build: ## Build with Stack
	stack build
.PHONY: build

test: ## Run the tests
	# stack build --fast --test --test-arguments=--format=progress -j4
	stack build --fast --test
.PHONY: test

repl: ## Run a REPL for development
	stack ghci
.PHONY: repl

repl-test: ## Run a REPL with tests
	stack ghci :sandbox-hs-test
.PHONY: repl-test

run: ## Run Krypton locally
	stack exec -- sandbox-hs-exe
.PHONY: run

ghcid: $(GHCID_BIN) ## Verifies quickly if project compiles on file change
	ghcid --command "make repl"
.PHONY: ghcid

test-ghcid: $(GHCID_BIN) ## Verifies quickly if project compiles on file change for tests
	ghcid --command "make repl-test"
.PHONY: ghcid

help: ## Display this message
	@echo "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/\\x1b[36m\1\\x1b[m:\2/' | column -c2 -t -s :)"
.PHONY: help
.DEFAULT_GOAL := help
