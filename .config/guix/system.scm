;; Divya's GNU Guix System Configuration files.
;; It lives at ~/.config/guix/system.scm
;; This should be reconfigured with `guix system reconfigure system.scm`.

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-modules (gnu services base)
 	     (gnu services sound)
	     (gnu services cups)
	     (gnu services mcron)
	     (gnu services shepherd)
	     (gnu services xorg))
(use-modules (gnu packages haskell-apps)
	     (gnu packages cups)
	     (nongnu packages linux) ;; For mainline linux, since AMD Ryzen 7 APU doesn't seem to work with linux-libre
	     (nongnu system linux-initrd)
	     (nongnu packages printers) ;; For non-free hp drivers
	     (btv rust) ;; For latest rust that is not yet in guix
	     (divya-lambda emacs)
	     (gnu packages version-control))
(use-modules (guix))
(use-service-modules cups desktop networking ssh xorg sddm)
(use-package-modules ssh)

;;; MCron Services for Scheduled Jobs

;; (define essential-backup-job
;;   ;; Hourly run a backup script in ~/.dotfiles that backs up notes, dotfiles,
;;   ;; and anything else into another disk and encrypts whatever is private.
;;   (job "0 */4 * * *"
;; 	 "./~/.dotfiles/scripts/backup"))

;; (define garbage-collector-job
;;   ;; Collect garbage 5 minutes after 09:00 AM UTC every day
;;   #~(job "5 0 * * *"
;; 	 "guix gc -F 1G"))

;;; Kmonad
(define kmonad-service
  (simple-service
   'kmonad-service
   shepherd-root-service-type
   (list (shepherd-service
	  (documentation "Run the kmonad daemon (kmonad-daemon)." )
	  (provision '(kmonad-daemon))
	  (requirement '(udev user-processes))
	  (start #~(make-forkexec-constructor
		    (list #$(file-append kmonad "/bin/kmonad")
			  (string-append
			   "/home/divya/.config/kmonad/divya.kbd"))
                    #:log-file "/var/log/kmonad.log"))
          (stop #~(make-kill-destructor))))))

(operating-system
 (locale "en_US.utf8")
 (timezone "UTC")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "lambda")

;; Kernel level modifications
;;(kernel-arguments '("modprobe.blacklist=nouveau"
;;		     "nvidia_drm.modeset=1"))

(groups (cons*
	  (user-group
	    (name "realtime")
	    (system? #t))
	  %base-groups))

 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "divya")
                (comment "Divya")
                (group "users")
                (home-directory "/home/divya")
                (supplementary-groups '("lp" "lpadmin" "wheel" "realtime" "netdev" "audio" "video")))
	       %base-user-accounts))

 ;; Packages installed system-wide.  Users can also install packages
 ;; under their own account: use 'guix search KEYWORD' to search
 ;; for packages and 'guix install PACKAGE' to install a package.
 (packages
  (append (map (compose list specification->package+output)
	       `( ;;; Wayland
		 ;; "wayland"
		 ;; "sway"

;;; Window Managers
		 ;; "awesome"
		 "stumpwm"
		 ;; "xmonad"

;;; Text Editors
		 ;; "emacs-next"
		 ;; "emacs-master-xwidgets"
		 "emacs-master-lucid"
		 "vim"

;;; Audio/Video/Streaming
		 "alsa-utils"
		 "alsa-lib"
		 "bluez"
		 "bluez-alsa"
		 "mpd"
		 "mpv"
		 "vlc"
		 "simplescreenrecorder"
		 ;; "carla"

;;; Connectivity
		 "blueman"
		 "v4l2loopback-linux-module" ; Webcam

;;; Browser
		 "librewolf"
		 "ungoogled-chromium"

;;; Email
		 ;; "mu"
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
		 "font-google-noto-emoji"
		 "font-lohit"

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
		 "gdb"
		 "ghc"
		 "cabal-install"
		 "hlint"
		 "gforth"
		 ;; "java"
		 "python"
		 "python-pip"
		 "r"
		 "sbcl"
		 ;;"rust-next" ;; from (btv rust)
		 ;; "rust-next:tools"
		 ;; "rust-next:cargo"
		 "rust"
		 "rust:rust-src"
		 "rust-cargo"
		 "rust-analyzer"
		 "rust-clippy"
		 "clojure"
		 "clojure-tools"
		 "go"
		 "gopls"
		 "gprolog"
		 ;; "swi-prolog"
		 ;; "smlnj"
		 ;; "polyml"
		 "opam"
		 "ocaml"
		 "sqlite"

;;; Terminals
		 "alacritty"
		 "xst"
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
		 "gvfs"		     ; For trash and remote management
		 "lf"

		 ;; Clipboard
		 "xclip"

;;; Search
		 "fd"
		 "fzf"
		 "ripgrep"

;;; Screenshot
		 "maim"

		 ;; Pritning
		 "hplip"
		 ;; "hplip-plugin"
		 "hplip-minimal"
		 "system-config-printer"

;;; Syncing
		 ;; "syncthing"
		 ;; "syncthing-gtk"
		 "rsync"

;;; Version Control/Package Management
		 "git"
		 "git:send-email"
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
		 "zip"
		 "tree"
		 "ncurses"

		 ;; Xorg
		 "xset"

		 ;; Graphics Drivers
		 "mesa"
		 "mesa-utils"
		 "xf86-video-nouveau"))
	  %base-packages))

 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append (list
           ;; To configure OpenSSH, pass an 'openssh-configuration'
           ;; record as a second argument to 'service' below.
           (service openssh-service-type)
           (service tor-service-type)
           (service cups-service-type
		    (cups-configuration
		     (web-interface? #t)
		     (extensions
		      (list cups-filters hplip-minimal))))

	   ;; Audio
	   ;; (service alsa-service-type)
	   ;; (service pulseaudio-service-type) Already included in %desktop-services

           (set-xorg-configuration
            (xorg-configuration (keyboard-layout keyboard-layout)))

	   (service pam-limits-service-type
		    (list
		     (pam-limits-entry "@realtime" 'both 'rtprio 99)
		     (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

	   kmonad-service)
	  %desktop-services
	  ;; (service nvidia-service-type)
	  ;; (set-xorg-configuration
	  ;;  (xorg-configuration
	   ;;  (modules (cons nvda %default-xorg-modules))
	    ;; (drivers '("nvidia")))))

	  ;; mcron services
	  ;; (simple-service 'lambda-cron-jobs
	  ;; 		  mcron-service-type
	  ;; 		  (garbage-collector-job))
          ))

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
		 (linux-arguments '("root=/dev/sda6"))
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
		      %base-file-systems)))
