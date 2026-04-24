# Upstream sources pinned here. Bump WPT_COMMIT when refreshing.
WPT_REPO    := https://github.com/web-platform-tests/wpt.git
WPT_COMMIT  := f900489fca393464f3379d7952d227997318b851
WPT_PATHS   := css/css-syntax

KUHN_URL    := https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt

VECTORS_DIR := test/vectors

.PHONY: update-vectors update-wpt update-utf8

update-vectors: update-wpt update-utf8
	@printf "\nvendored vectors refreshed. Review the diff and update the\n"
	@printf "commit SHA in $(VECTORS_DIR)/README.md if it changed.\n"

update-wpt:
	@set -eu; \
	tmp=$$(mktemp -d); \
	trap 'rm -rf "$$tmp"' EXIT; \
	echo "Cloning WPT @ $(WPT_COMMIT) into $$tmp ..."; \
	git -C "$$tmp" init -q; \
	git -C "$$tmp" remote add origin $(WPT_REPO); \
	git -C "$$tmp" config core.sparseCheckout true; \
	for p in $(WPT_PATHS); do echo "$$p/*"; done \
	  > "$$tmp/.git/info/sparse-checkout"; \
	git -C "$$tmp" fetch --depth=1 origin $(WPT_COMMIT) -q; \
	git -C "$$tmp" checkout -q FETCH_HEAD; \
	rm -rf $(VECTORS_DIR)/wpt; \
	mkdir -p $(VECTORS_DIR)/wpt; \
	for p in $(WPT_PATHS); do \
	  dst_name=$$(basename "$$p"); \
	  cp -R "$$tmp/$$p" "$(VECTORS_DIR)/wpt/$$dst_name"; \
	done; \
	rm -rf $(VECTORS_DIR)/wpt/css-syntax/charset

update-utf8:
	@echo "Fetching $(KUHN_URL) ..."
	@mkdir -p $(VECTORS_DIR)/utf8
	@curl -sSLf -o $(VECTORS_DIR)/utf8/UTF-8-test.txt $(KUHN_URL)
