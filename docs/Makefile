PNGS := $(wildcard artwork/*.png)
MP4S := $(wildcard artwork/*.mp4)

JPEGS := $(patsubst artwork/%.png,src/assets/img/%.jpg,$(PNGS))
GIFS := $(patsubst artwork/%.mp4,src/assets/img/%.gif,$(MP4S))

COMMANDS_JSON := src/_data/commands.json
MNIST_EXPERT_SKEL := src/_includes/Guild-mnist-expert
FAVICON := src/favicon.ico
ALL := $(JPEGS) $(GIFS) $(COMMANDS_JSON) $(MNIST_EXPERT_SKEL) $(FAVICON)

GUILD := ../scripts/guild-dev

build: $(ALL)
	bundle exec jekyll build

src/assets/img/%.jpg: artwork/%.png artwork/%.opts
	convert $(word 1,$^) `cat $(word 2,$^)` $@

src/assets/img/%.gif: artwork/%.mp4 artwork/%.opts
	ffmpeg -i $(word 1,$^) -vf "palettegen" -y /tmp/guild-docs-palettegen.png
	ffmpeg -i $(word 1,$^) -i /tmp/guild-docs-palettegen.png -lavfi "`cat $(word 2,$^)` paletteuse" -y $@

$(COMMANDS_JSON):
	$(GUILD) cmds-json > $@

$(MNIST_EXPERT_SKEL):
	guild init --print \
	  --name "MNIST Expert" \
	  --train-cmd mnist_with_summaries \
	  > $@

clean:
	bundle exec jekyll clean
	rm Gemfile.lock
	rm -f $(ALL)

serve:
	bundle exec jekyll serve --limit_posts=1 --incremental --watch

$(FAVICON): src/apple-touch-icon.png
	convert $< \
	\( -clone 0 -resize 16x16 \) \
	\( -clone 0 -resize 32x32 \) \
	\( -clone 0 -resize 48x48 \) \
	\( -clone 0 -resize 64x64 \) \
	-delete 0 \
	$@

spell-list:
	@for f in `find -name "*.md"`; do \
	  if aspell list -p ./aspell-words < "$$f" | grep -v "^$$"; then \
	    echo "ERROR: $$f contains unknown words - see list above"; \
	    exit 1; \
	  fi \
	done

spell-check:
	@for f in `find -name "*.md"`; do \
	  aspell check -p ./aspell-words "$$f"; \
	done

test:
	@echo $(ALL)
