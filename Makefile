.PHONY: all install uninstall

SYSTEMD_USER_DIR := $(HOME)/.config/systemd/user
SERVICE_FILE := podman.service

all: install

install:
	@mkdir -p $(SYSTEMD_USER_DIR)
	@cp $(SERVICE_FILE) $(SYSTEMD_USER_DIR)/$(SERVICE_FILE)
	@systemctl --user daemon-reload
	@systemctl --user enable $(SERVICE_FILE)
	@systemctl --user start $(SERVICE_FILE)

uninstall:
	@-systemctl --user stop $(SERVICE_FILE) 2>/dev/null || true
	@-systemctl --user disable $(SERVICE_FILE) 2>/dev/null || true
	@rm -f $(SYSTEMD_USER_DIR)/$(SERVICE_FILE)
	@systemctl --user daemon-reload
