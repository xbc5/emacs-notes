Decrypt `key.asc` to get the `git-crypt` symmetric key; use that to transparently decrypt secrets/.

## Commands

### Makefile Commands

| Command   | Description                                                                                         |
| --------- | --------------------------------------------------------------------------------------------------- |
| all       | Build and install the podman service (default target).                                              |
| install   | Install podman.service to ~/.config/systemd/user/, reload systemd, enable and start the service.    |
| uninstall | Stop and disable the podman service, then remove it from the systemd user directory.                |
