;; Divya's GNU Guix System Configuration files.
;; It lives at ~/.config/guix/system.scm
;; This should be reconfigured with `guix system reconfigure system.scm`.

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-service-modules cups desktop networking ssh xorg)

(operating-system
 (locale "en_IN.utf8")
 (timezone "Europe/London")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "lambda")

 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "divya")
                (comment "Divya")
                (group "users")
                (home-directory "/home/divya")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 ;; Packages installed system-wide.  Users can also install packages
 ;; under their own account: use 'guix search KEYWORD' to search
 ;; for packages and 'guix install PACKAGE' to install a package.
 (packages (specification->packages
	    (list
	     ;;; Wayland
	     "wayland"
	     "sway"

	      ;;; Window Managers
	     "awesome"
	     "stumpwm"
	     "xmonad"

	      ;;; Text Editors
	     "emacs-next"
	     "vim"

	      ;;; Audio/Video/Streaming
	     "pipewire"
	     "wireplumber"
	     "qpwgraph"
	     "easyeffects"
	     "mpd"
	     "mpv"
	     "vlc"
	     "ffmpeg"
	     "pulsemixer"
	     "simplescreenrecorder"
	     "carla"

	      ;;; Browser
	     "librewolf"
	     "isync"
	     "msmtp"

	      ;;; Graphics/Image
	     "imagemagick"
	     "gimp"
	     "krita"

	      ;;; Mathematics/Computational Software
	     "sage"
	     "octave"
	     "gnuplot"

	      ;;; LaTeX
	     "texlive"
	     "texlive-xetex"
	     "texmacs"

	      ;;; Programming Languages
	     "chez-scheme"
	     "mit-scheme"
	     "racket"
	     "gcc-toolchain"
	     "gforth"
	     "python"
	     "python-pip"
	     "r"
	     "sbcl"
	     "rust-rustup-toolchain"
	     "clojure"
	     "clojure-tools"
	     "gprolog"
	     "swi-prolog"
	     "smlnj"
	     "polyml"
	     "opam"
	     "sqlite"

	      ;;; Terminals
	     "alacritty"
	     "foot"

	      ;;; File Manager
	     "thunar"
	     "lf"

	      ;;; Search
	     "fzf"
	     "ripgrep"

	      ;;; Screenshot
	     "maim"

	      ;;; Syncing
	     "syncthing"
	     "syncthing-gtk"
	     "rsync"

	      ;;; Version Control/Package Management
	     "git"
	     "stow"

	      ;;; Shell
	     "bash"
	     "zsh"
	     "scsh"

              ;;; Keyboard Management
	     "kmonad"

	       ;;; Writing/Reading
	     "zathura"
	     "zathura-ps"
	     "zathura-pdf-mupdf"
	     "zathura-djvu"
	     ;;"pdftk"
	     "mupdf"
	     "evince"
	     "djvu2pdf"
	     "xournalpp"
	     ;;("pdf2txt")
	     "pandoc"
	     "libreoffice"

	      ;;; Utilities
	     "curl")
            %base-packages))

 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append (list
           ;; To configure OpenSSH, pass an 'openssh-configuration'
           ;; record as a second argument to 'service' below.
           (service openssh-service-type)
           (service tor-service-type)
           (service cups-service-type)
           (set-xorg-configuration
            (xorg-configuration (keyboard-layout keyboard-layout))))

          ;; This is the default list of services we
          ;; are appending to.
          %desktop-services))

 (bootloader (bootloader-configuration
              (bootloader grub-bootloader)
              (targets '("/dev/sda"))
	      (default-entry 0)
	      (menu-entries
	       (list
		(menu-entry
		 (label "arch-root")
		 (device (uuid "886fb01f-323f-40ab-9434-9f00feb96446" 'ext4))
		 (linux "/boot/vmlinuz-linux-rt")
		 (linux-arguments '("root=/dev/sdb1"))
		 (initrd "/boot/initramfs-linux-rt-fallback.img"))))))
 ;;(menu-entry
 ;; (label "guix-root")
 ;; (linux-arguments '("root=/dev/sda8")))))))
 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/")
                       (device (uuid
				"30dfe5c0-8180-4993-bb11-8a1476f3a003"
				'ext4))
                       (type "ext4")) %base-file-systems)))
