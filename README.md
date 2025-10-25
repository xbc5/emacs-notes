Decrypt `key.asc` to get the `git-crypt` symmetric key; use that to transparently decrypt secrets/.

## Documentation

- [Email Setup](docs/email.md) - Complete guide for setting up the email system with Proton Mail Bridge, mbsync, and mu4e

## Commands

### Makefile Commands

| Command             | Description                                                                                         |
| ------------------- | --------------------------------------------------------------------------------------------------- |
| all                 | Install the complete email system (default target).                                                 |
| install-doom-emacs  | Install Doom Emacs and add ~/.emacs.d/bin to PATH (skips if already installed).                    |
| install-email       | Install all email services, configure systemd, install the email script, and set up PATH.           |
| uninstall-email     | Stop and disable all email services, then remove service files and clean up.                        |
