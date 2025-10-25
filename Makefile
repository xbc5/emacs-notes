.PHONY: all install uninstall

SYSTEMD_USER_DIR := $(HOME)/.config/systemd/user
PODMAN_SERVICE := podman.service
EMACS_SERVICE := emacs.service
PROTON_MAIL_BRIDGE_SERVICE := proton-mail-bridge.service
XDG_DATA_HOME ?= $(HOME)/.local/share
DATA_DIR := $(XDG_DATA_HOME)/emacs-email
MAIL_DIR := $(HOME)/.mail/proton-mail
COMPOSE_FILE := docker-compose.yml

all: install-email

install-email:
	# - PRECONDITIONS -
	@missing=""; \
	command -v podman >/dev/null 2>&1 || missing="$$missing podman"; \
	command -v mu >/dev/null 2>&1 || missing="$$missing mu"; \
	command -v mbsync >/dev/null 2>&1 || missing="$$missing mbsync"; \
	if [ -n "$$missing" ]; then \
		echo "Error: Missing required dependencies:$$missing" >&2; \
		exit 1; \
	fi
	# - PODMAN SERVICE FILE -
	@mkdir -p $(SYSTEMD_USER_DIR)
	@cp $(PODMAN_SERVICE) $(SYSTEMD_USER_DIR)/$(PODMAN_SERVICE)
	@systemctl --user daemon-reload
	@systemctl --user enable $(PODMAN_SERVICE)
	@systemctl --user start $(PODMAN_SERVICE)
	# - EMACS SERVICE FILE -
	@cp $(EMACS_SERVICE) $(SYSTEMD_USER_DIR)/$(EMACS_SERVICE)
	@systemctl --user daemon-reload
	@systemctl --user enable $(EMACS_SERVICE)
	@systemctl --user start $(EMACS_SERVICE)
	# - COMPOSE FILE -
	@mkdir -p $(DATA_DIR)
	@cp $(COMPOSE_FILE) $(DATA_DIR)/$(COMPOSE_FILE)
	# - MAIL DIRECTORY -
	@mkdir -p $(MAIL_DIR)
	# - SYNC WITH REMOTE ACCOUNT -
	#@podman run --rm -it -v protonmail:/root shenxn/protonmail-bridge init
	# - PROTON MAIL BRIDGE SERVICE FILE -
	@cp $(PROTON_MAIL_BRIDGE_SERVICE) $(SYSTEMD_USER_DIR)/$(PROTON_MAIL_BRIDGE_SERVICE)
	@systemctl --user daemon-reload
	@systemctl --user enable $(PROTON_MAIL_BRIDGE_SERVICE)
	@systemctl --user start $(PROTON_MAIL_BRIDGE_SERVICE)

uninstall-email:
	# - PRECONDITIONS -
	@-systemctl --user stop $(PODMAN_SERVICE) 2>/dev/null || true
	@-systemctl --user disable $(PODMAN_SERVICE) 2>/dev/null || true
	@-systemctl --user stop $(EMACS_SERVICE) 2>/dev/null || true
	@-systemctl --user disable $(EMACS_SERVICE) 2>/dev/null || true
	@-systemctl --user stop $(PROTON_MAIL_BRIDGE_SERVICE) 2>/dev/null || true
	@-systemctl --user disable $(PROTON_MAIL_BRIDGE_SERVICE) 2>/dev/null || true
	# - UNINSTALL -
	@rm -f $(SYSTEMD_USER_DIR)/$(PODMAN_SERVICE)
	@rm -f $(SYSTEMD_USER_DIR)/$(EMACS_SERVICE)
	@rm -f $(SYSTEMD_USER_DIR)/$(PROTON_MAIL_BRIDGE_SERVICE)
	@rm -f $(DATA_DIR)/$(COMPOSE_FILE)
	# - FINISH -
	@systemctl --user daemon-reload
