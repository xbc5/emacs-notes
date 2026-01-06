.PHONY: all install-doom-emacs sync-proton-mail-bridge install-email uninstall-email

SYSTEMD_USER_DIR := $(HOME)/.config/systemd/user
PODMAN_SERVICE := podman.service
PROTON_MAIL_BRIDGE_SERVICE := proton-mail-bridge.service
XDG_DATA_HOME ?= $(HOME)/.local/share
DATA_DIR := $(XDG_DATA_HOME)/emacs-email
MAIL_DIR := $(HOME)/.mail/proton-mail
CONFIG_DIR := $(HOME)/.config/emacs-email
EMACS_DIR := $(HOME)/.emacs.d
EMACS_BIN_DIR := $(HOME)/.emacs.d/bin
BIN_DIR := $(HOME)/.local/bin
EMAIL_SCRIPT := email
ZSHRC := $(HOME)/.zshrc
COMPOSE_FILE := docker-compose.yml

all: install-email

install-doom-emacs:
	@# - CHECK IF DOOM EMACS ALREADY EXISTS -
	@if [ -d $(EMACS_DIR) ]; then \
		echo "Doom Emacs already installed ($(EMACS_DIR) exists), skipping..."; \
	else \
		echo "Installing Doom Emacs..."; \
		git clone --depth 1 https://github.com/doomemacs/doomemacs $(HOME)/.emacs.d; \
		$(EMACS_BIN_DIR)/doom install; \
		$(EMACS_BIN_DIR)/doom sync; \
	fi
	@# - PATH CONFIGURATION -
	@if ! grep -q '\.emacs\.d/bin' $(ZSHRC) 2>/dev/null; then \
		echo 'export PATH="$$HOME/.emacs.d/bin:$$PATH"' >> $(ZSHRC); \
	fi

sync-proton-mail-bridge:
	@# - SYNC WITH REMOTE ACCOUNT -
	podman run --rm -it -v protonmail:/root shenxn/protonmail-bridge init

install-email: install-doom-emacs
	@# - PRECONDITIONS -
	@missing=""; \
	command -v podman >/dev/null 2>&1 || missing="$$missing podman"; \
	command -v mu >/dev/null 2>&1 || missing="$$missing mu"; \
	command -v mbsync >/dev/null 2>&1 || missing="$$missing mbsync"; \
	command -v emacs-lucid >/dev/null 2>&1 || missing="$$missing emacs-lucid"; \
	if [ -n "$$missing" ]; then \
		echo "Error: Missing required dependencies:$$missing" >&2; \
		exit 1; \
	fi

	@# - PODMAN SERVICE FILE -
	@mkdir -p $(SYSTEMD_USER_DIR)
	cp $(PODMAN_SERVICE) $(SYSTEMD_USER_DIR)/$(PODMAN_SERVICE)
	systemctl --user daemon-reload
	systemctl --user enable $(PODMAN_SERVICE)
	systemctl --user start $(PODMAN_SERVICE)

	@# - COMPOSE FILE -
	@mkdir -p $(DATA_DIR)
	cp $(COMPOSE_FILE) $(DATA_DIR)/$(COMPOSE_FILE)

	@# - DIRECTORIES -
	@mkdir -p $(MAIL_DIR)
	@mkdir -p $(CONFIG_DIR)

	@# - PROTON MAIL BRIDGE SERVICE FILE -
	cp $(PROTON_MAIL_BRIDGE_SERVICE) $(SYSTEMD_USER_DIR)/$(PROTON_MAIL_BRIDGE_SERVICE)
	systemctl --user daemon-reload
	systemctl --user enable $(PROTON_MAIL_BRIDGE_SERVICE)
	systemctl --user start $(PROTON_MAIL_BRIDGE_SERVICE)

	@# - EMAIL SCRIPT -
	@mkdir -p $(BIN_DIR)
	@install -m 755 $(EMAIL_SCRIPT) $(BIN_DIR)/$(EMAIL_SCRIPT)

	@# - PATH CONFIGURATION -
	@if ! grep -q '\.local/bin' $(ZSHRC) 2>/dev/null; then \
		echo 'export PATH="$$HOME/.local/bin:$$PATH"' >> $(ZSHRC); \
	fi

	@# - INSTALLATION COMPLETE -
	@echo ""
	@echo "Installation complete!"
	@echo ""
	@echo "Next steps:"
	@echo "  1. Create ~/.mbsyncrc with your mail sync configuration"
	@echo "  2. Create ~/.authinfo with your authentication credentials"
	@echo "  3. Create ~/.config/emacs-email/conf.el with your Emacs email configuration"
	@echo "  4. Run 'mu init --my-address foo@protonmail.com --maildir=~/.mail' to set the necessary 'mu' metadata"
	@echo "  5. Run 'mbsync -a' to synchronize mail"
	@echo "  6. Run 'mu index' to index mail"
	@echo "  7. Run 'email start' to start the email system"
	@echo ""
	@echo "See docs/email.md for detailed configuration instructions."
	@echo ""

uninstall-email:
	@# - PRECONDITIONS -
	@-systemctl --user stop $(PODMAN_SERVICE) 2>/dev/null || true
	@-systemctl --user disable $(PODMAN_SERVICE) 2>/dev/null || true
	@-systemctl --user stop $(PROTON_MAIL_BRIDGE_SERVICE) 2>/dev/null || true
	@-systemctl --user disable $(PROTON_MAIL_BRIDGE_SERVICE) 2>/dev/null || true
	@# - UNINSTALL -
	@rm -f $(SYSTEMD_USER_DIR)/$(PODMAN_SERVICE)
	@rm -f $(SYSTEMD_USER_DIR)/$(PROTON_MAIL_BRIDGE_SERVICE)
	@rm -f $(DATA_DIR)/$(COMPOSE_FILE)
	@# - FINISH -
	@systemctl --user daemon-reload
