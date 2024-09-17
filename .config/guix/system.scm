;; Divya's GNU Guix System Configuration files.
;; It lives at ~/.config/guix/system.scm
;; This should be reconfigured with `guix system reconfigure system.scm`.

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-modules (gnu services sound))
(use-service-modules cups desktop networking ssh xorg)
(use-package-modules ssh)

(operating-system
 (locale "en_US.utf8")
 (timezone "UTC")
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
 (packages (append (map specification->package
			'(
	                  ;;; Wayland
			  ;; "wayland"
			  ;; "sway"

	                  ;;; Window Managers
			  ;; "awesome"
			  "stumpwm"
			  ;; "xmonad"

	                  ;;; Text Editors
			  "emacs-next"
			  "vim"

	                  ;;; Audio/Video/Streaming
			  ;; "alsamixer"
			  "alsa-utils"
			  "alsa-lib"
			  "bluez-alsa"
			  "pipewire"
			  "pavucontrol"
			  "wireplumber"
			  "qpwgraph"
			  "easyeffects"
			  "mpd"
			  "mpv"
			  "vlc"
			  "pulsemixer"
			  "simplescreenrecorder"
			  "carla"

	                  ;;; Connectivity
			  "blueman"
			  "v4l2loopback-linux-module" ; Webcam

	                  ;;; Browser
			  "librewolf"

	                  ;;; Email
			  "mu"
			  "isync"
			  "msmtp"

	                  ;;; Graphics/Image
			  "imagemagick"
			  "gimp"
			  ;; "krita"
			  "imlib2"
			  "feh"

	                  ;;; Fonts
			  "font-iosevka"
			  "fontmanager"

	                  ;;; Mathematics/Computational Software
			  ;; "sage"
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
			  "ghc"
			  "cabal-install"
			  "hlint"
			  "gforth"
			  "python"
			  "python-pip"
			  "r"
			  "sbcl"
			  "rust"
			  "rust-cargo"
			  "rust-analyzer"
			  "rust-clippy"
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
			  "st"
			  ;; "foot"

	     		  ;;; Launchers
			  "dmenu"

	     		  ;;; Notifications
			  "dunst"

	     		  ;;; Theme
			  "lxappearance"

	     ;;; File Manager
			  "thunar"
			  "thunar-volman"
			  "gvfs" ; For trash and remote management
			  "lf"

	     ;;; Search
			  "fd"
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
			  "flatpak"
	     ;;; Shell
			  "bash"
			  "zsh"
			  "scsh"

             ;;; Keyboard Management
			  "kmonad"

	     ;;; Password/Security
			  "keepassxc"
			  "password-store"

	      ;;; Utilities
			  "curl"
			  "ntfs-3g" ;; for mounting ntfs file systems
			  ))
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
	   ;; Audio
	   ;; (service alsa-service-type)
	   ;; (service pulseaudio-service-type)

           (set-xorg-configuration
            (xorg-configuration (keyboard-layout keyboard-layout))))

          ;; This is the default list of services we
          ;; are appending to.
          %desktop-services))

 (bootloader (bootloader-configuration
              (bootloader grub-bootloader)
              (targets '("/dev/sdb"))
	      (default-entry 0)
	      (menu-entries
	       (list
		(menu-entry
		 (label "arch-root")
		 (linux "/boot/vmlinuz-linux-rt")
		 (device (uuid "886fb01f-323f-40ab-9434-9f00feb96446" 'ext4))
		 (linux-arguments '("root=/dev/sda7"))
		 (initrd "/boot/initramfs-linux-rt.img"))))))

 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/")
                       (device (uuid
				"50e2756a-3b56-4f1f-a056-ed0e88f277d2"
				'btrfs))
                       (type "btrfs"))

		      (file-system
		       (mount-point "/mnt/arch")
		       (device (uuid
				"886fb01f-323f-40ab-9434-9f00feb96446"
				'ext4))
		       (type "ext4"))

		      ;; (file-system
		      ;; 	(mount-point "/mnt/LDisk-D")
		      ;; 	(device (uuid
		      ;; 		  "42E6712AE6711F7B"
		      ;; 		  'ntfs))
		      ;; 	(flags '(bind-mount))
		      ;; 	(type "ntfs"))

		      ;; (file-system
		      ;; 	(mount-point "/mnt/LDisk-E")
		      ;; 	(device (uuid
		      ;; 		  "FA5CCCEF5CCCA7A9"
		      ;; 		  'ntfs))
		      ;; 	(type "ntfs"))
		      %base-file-systems)))
