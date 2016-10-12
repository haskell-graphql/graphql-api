.PHONY: check clean docs format lint

check:
	stack test --fast

clean:
	stack clean

docs:
	stack haddock

format:
	./scripts/hindent-everything

lint:
	hlint -q .
