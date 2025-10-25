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

**IMPORTANT:** You must run `make sync-proton-mail-bridge` before running `make install-email`.

Run the installation command:

```bash
git clone https://github.com/xbc5/emacs-notes ~/.doom.d \
  && cd ~/.doom.d \
  && make install-email
```

This will install Doom Emacs (if not already installed) and all necessary scripts and services. It will not install sensitive configuration files, which you must create manually as described in the Required Configuration Files section.

## Required Configuration Files

After installation, you need to create three configuration files:

### 1. ~/.mbsyncrc

This file configures mbsync to synchronize mail between Proton Mail Bridge and local storage. You can simply change "foo" with your username using sed.

Example configuration:

```
# ----- REMOTE ------------------------------------------------------------------
# IMAPStore: Define a remore connection and assign it an ID.
IMAPStore foo@protonmail.com-remote # Arbitrary identifier
Host 127.0.0.1 # The remote server's hostname
Port 1143
User foo@protonmail.com # Your email address
PassCmd "cat ~/.config/emacs-mail/bridge-password"
AuthMechs LOGIN
TLSType None

# ----- LOCAL -------------------------------------------------------------------
# MaildirStore: Defines local storage in Maildir format (a standard Unix mail format where each
# email is a separate file in directories). This is what mu and mu4e expect on your computer.
MaildirStore foo@protonmail.com-local
Path ~/.mail/foo@protonmail.com/
Inbox ~/.mail/foo@protonmail.com/INBOX
# Preserve exact folder structure from remote server
SubFolders Verbatim

# ----- SYNC --------------------------------------------------------------------
# A channel is a named identifier that associates remote and local stores.
# For example, you can run `mbsync foo@protonmail.com` and it will sync from foo@protonmail.com-remote,
# into the foo@protonmail.com-local folder. These are also identifiers, and they have their own
# configuration options. MaildirStore defines foo@protonmail.com-local, and IMAPStore defines
# foo@protonmail.com-remote.
Channel foo@protonmail.com
Far :foo@protonmail.com-remote: # Master
Near :foo@protonmail.com-local: # Slave
# Patterns: Specify which folders to sync. Useful cases:
#   "* !Archives !Spam" — sync everything except old/spam folders (saves space)
#   "INBOX Drafts Sent" — sync only essential folders (faster, minimal storage)
#   "*" — sync everything (what you have here)
Patterns * !Archives
# Create missing folders on both sides if needed
Create Both
# Delete messages on both sides if removed on the other
Expunge Both
# SyncState: Store sync metadata so mbsync knows what's already been synced.
# Without this, every sync would unnecessarily retransfer everything.
# "*" means store state for all folders. Other options: specific folder names, or omit entirely
# (but omitting means slower syncs and potential duplicates on resync).
SyncState *
```

### 2. ~/.authinfo

This file stores authentication credentials for various remote services and is commonly used by Emacs packages. You need to insert your SMTP credentials here. Since these credentials are only for the local bridge, the file does not need to be encrypted. However, you can append a `.gpg` extension if you prefer, and Emacs will automatically decrypt it when needed.

```
machine 127.0.0.1 login foo@protonmail.com password your-bridge-password
machine 127.0.0.1 login bar@protonmail.com password your-bridge-password
```

Set proper permissions to protect your credentials:

```bash
chmod 600 ~/.authinfo
```

To retrieve your Proton Mail Bridge credentials, use the `email cli` command to access the Proton Bridge Shell, where you will find your credentials.

### 3. ~/.config/emacs-email/conf.el

This file contains Emacs-specific email configuration for mu4e. You can simply change "foo" with your username.

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

1. Run `make sync-proton-mail-bridge` to sync with your Proton Mail account (must be run before install-email)
2. Run `make install-email`
3. Create `~/.authinfo` with your bridge credentials
4. Create `~/.mbsyncrc` with your sync configuration
5. Create `~/.config/emacs-email/conf.el` with your Emacs configuration
6. Ensure the mail service is running
7. Run `mu init --my-address foo@protonmail.com --maildir ~/.mail` to set the necessary `mu` metadata
8. Run `mbsync -a` to pull mail from the bridge
9. Run `mu index` to index the unencrypted mail
10. Start the email system with `email start`

## Systemd Services

The following systemd user services are installed:

- **podman.service**: Runs the Podman API service
- **emacs.service**: Runs the Emacs daemon
- **proton-mail-bridge.service**: Runs Proton Mail Bridge (depends on podman.service)

