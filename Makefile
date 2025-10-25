.PHONY: all install uninstall

SYSTEMD_USER_DIR := $(HOME)/.config/systemd/user
SERVICE_FILE := podman.service
XDG_DATA_HOME ?= $(HOME)/.local/share
DATA_DIR := $(XDG_DATA_HOME)/emacs-email
COMPOSE_FILE := docker-compose.yml

all: install

install:
	# - SERVICE FILE -
	@mkdir -p $(SYSTEMD_USER_DIR)
	@cp $(SERVICE_FILE) $(SYSTEMD_USER_DIR)/$(SERVICE_FILE)
	@systemctl --user daemon-reload
	@systemctl --user enable $(SERVICE_FILE)
	@systemctl --user start $(SERVICE_FILE)
	# - COMPOSE FILE -
	@mkdir -p $(DATA_DIR)
	@cp $(COMPOSE_FILE) $(DATA_DIR)/$(COMPOSE_FILE)

uninstall:
	# - PRECONDITIONS -
	@-systemctl --user stop $(SERVICE_FILE) 2>/dev/null || true
	@-systemctl --user disable $(SERVICE_FILE) 2>/dev/null || true
	# - UNINSTALL -
	@rm -f $(SYSTEMD_USER_DIR)/$(SERVICE_FILE)
	@rm -f $(DATA_DIR)/$(COMPOSE_FILE)
	# - FINISH -
	@systemctl --user daemon-reload
