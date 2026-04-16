QUARTO ?= $(shell if command -v quarto >/dev/null 2>&1; then command -v quarto; elif [ -x /Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto ]; then printf '%s' /Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto; else printf '%s' quarto; fi)

default: preview

all: build

preview:
	$(QUARTO) preview --no-browser --port 8003

build:
	$(QUARTO) render

deploy:
	git push

clean:
	rm -rf _site
	rm -rf _freeze
