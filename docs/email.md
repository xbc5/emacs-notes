# Email Setup Documentation

This document provides detailed configuration instructions for setting up the email system.

## Overview

The email system consists of:
- **Emacs daemon**: Runs in the background for quick access
- **Podman**: Container runtime for running Proton Mail Bridge
- **Proton Mail Bridge**: Provides IMAP/SMTP bridge for Proton Mail
- **mbsync**: Synchronizes email between the bridge and local storage
- **mu**: Emacs email client
- **mu4e**: Emacs email client

## Installation

Run the installation command:

```bash
make install-email
```

This will install all necessary scripts and services. It will not install sensitive configuration files, which you must create manually as described in the Required Configuration Files section.

## Required Configuration Files

After installation, you need to create three configuration files:

### 1. ~/.mbsyncrc

This file configures mbsync to synchronize mail between Proton Mail Bridge and local storage.

Example configuration:

```
IMAPAccount proton
Host 127.0.0.1
Port 1143
User your-email@protonmail.com
PassCmd "cat ~/.authinfo | grep protonmail | awk '{print $NF}'"
SSLType None

IMAPStore proton-remote
Account proton

MaildirStore proton-local
Path ~/.mail/proton-mail/
Inbox ~/.mail/proton-mail/INBOX
SubFolders Verbatim

Channel proton
Far :proton-remote:
Near :proton-local:
Patterns *
Create Both
Expunge Both
SyncState *
```

### 2. ~/.authinfo

This file stores authentication credentials for various remote services and is commonly used by Emacs packages. You need to insert your SMTP credentials here. Since these credentials are only for the local bridge, the file does not need to be encrypted. However, you can append a `.gpg` extension if you prefer, and Emacs will automatically decrypt it when needed.

```
machine 127.0.0.1 login your-email@protonmail.com port 1143 password your-bridge-password
machine 127.0.0.1 login your-email@protonmail.com port 1025 password your-bridge-password
```

Set proper permissions to protect your credentials:

```bash
chmod 600 ~/.authinfo
```

To retrieve your Proton Mail Bridge credentials, use the `email cli` command to access the Proton Bridge Shell, where you will find your credentials.

### 3. ~/.config/emacs-email/conf.el

This file contains Emacs-specific email configuration for mu4e.

Example configuration:

```elisp
;; Each path is relative to the path of the maildir (~/.mail) you passed to `mu`.
(set-email-account!
  "foo@protonmail.com"
  '((mu4e-sent-folder       . "/foo@protonmail.com/Sent")
    (mu4e-drafts-folder     . "/foo@protonmail.com/Drafts")
    (mu4e-trash-folder      . "/foo@protonmail.com/Trash")
    (mu4e-refile-folder     . "/foo@protonmail.com/All Mail")
    (smtpmail-smtp-user     . "foo@protonmail.com")
    (mu4e-compose-signature . "\nJohn Doe")
    (smtpmail-smtp-user . "foo@protonmail.com")
    (smtpmail-smtp-server   . "127.0.0.1")
    (smtpmail-stream-type . plain)) ; TLS config (plain, ssl, or starttls). We don't need TLS for the local bridge.
  t)
```

## Using the Email System

The `email` script provides convenient commands for managing the email system: `start`, `stop`, and `cli`. The `cli` command stops the proton-mail-bridge service and opens the Proton Mail Bridge CLI for configuration.

## Initial Setup Workflow

1. Run `make install-email`
2. When prompted, choose 'y' to initialize Proton Mail Bridge
3. In the Bridge CLI, log in to your Proton Mail account
4. Create `~/.authinfo` with your bridge credentials
5. Create `~/.mbsyncrc` with your sync configuration
6. Create `~/.config/emacs-email/conf.el` with your Emacs configuration
7. Ensure the mail service is running
8. Run `mu init --my-address foo@protonmail.com --maildir=~/.mail` to set the necessary `mu` metadata
9. Run `mbsync -a` to pull mail from the bridge
10. Run `mu index` to index the unencrypted mail
11. Start the email system with `email start`

## Systemd Services

The following systemd user services are installed:

- **podman.service**: Runs the Podman API service
- **emacs.service**: Runs the Emacs daemon
- **proton-mail-bridge.service**: Runs Proton Mail Bridge (depends on podman.service)

