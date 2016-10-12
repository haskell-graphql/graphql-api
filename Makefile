.PHONY: check clean docs format image lint

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

image:
	./scripts/build-image $(shell ./scripts/image-tag)
