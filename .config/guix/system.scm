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
  (packages (append (list 
		      	  ;;; Wayland
		      	  (specification->package "wayland")
			  (specification->package "sway")

			  ;;; Window Managers
		      	  (specification->package "awesome")
			  (specification->package "stumpwm")
			  (specification->package "xmonad")

			  ;;; Text Editors
			  (specification->package "emacs-next")
			  (specification->package "vim")

			  ;;; Audio/Video/Streaming
			  (specification->package "pipewire")
			  (specification->package "wireplumber")
			  (specification->package "qpwgraph")
			  (specification->package "easyeffects")
			  (specification->package "mpd")
			  (specification->package "mpv")
			  (specification->package "vlc")
			  (specification->package "ffmpeg")
			  (specification->package "pulsemixer")
			  (specification->package "simplescreenrecorder")
			  (specification->package "carla")
			  			  
			  ;;; Browser
			  (specification->package "librewolf")
			  (specification->package "isync")
			  (specification->package "msmtp")
			  
			  ;;; Graphics/Image
			  (specification->package "imagemagick")
			  (specification->package "gimp")
			  (specification->package "krita")

			  ;;; Mathematics/Computational Software
			  ;; (specification->package "sage")
			  (specification->package "octave")
			  (specification->package "gnuplot")

			  ;;; LaTeX
			  (specification->package "texlive")
			  (specification->package "texlive-xetex")
			  (specification->package "texmacs")

			  ;;; Programming Languages
			  (specification->package "chez-scheme")
			  (specification->package "mit-scheme")
			  (specification->package "racket")
			  (specification->package "gcc-toolchain")
			  (specification->package "gforth")
			  (specification->package "r")
			  (specification->package "python")
			  (specification->package "python-pip")
			  (specification->package "sbcl")
			  (specification->package "rust-rustup-toolchain")
			  (specification->package "clojure")
			  (specification->package "clojure-tools")
			  (specification->package "gprolog")
			  (specification->package "swi-prolog")
			  (specification->package "smlnj")
			  (specification->package "polyml")
			  (specification->package "opam")
			  (specification->package "sqlite")
			  
			  ;;; Terminals
			  (specification->package "alacritty")
			  (specification->package "foot")
			  
			  ;;; File Manager
			  (specification->package "thunar")
			  (specification->package "lf")

			  ;;; Search
			  (specification->package "fzf")
			  (specification->package "ripgrep")

			  ;;; Screenshot
			  (specification->package "maim")

			  ;;; Syncing
			  (specification->package "syncthing")
			  (specification->package "syncthing-gtk")
			  (specification->package "rsync")

			  ;;; Version Control/Package Management
			  (specification->package "git")
			  (specification->package "stow")
			  
			  ;;; Shell
			  (specification->package "bash")
			  (specification->package "zsh")
			  (specification->package "scsh")

			  ;;; Keyboard Management
			  (specification->package "kmonad")

			  ;;; Writing/Reading
			  (specification->package "zathura")
			  (specification->package "zathura-ps")
			  (specification->package "zathura-pdf-mupdf")
			  (specification->package "zathura-djvu")
			  ;;(specification->package "pdftk")
			  (specification->package "mupdf")
			  (specification->package "evince")
			  (specification->package "djvu2pdf")
			  (specification->package "xournalpp")
			  ;;(specification->package "pdf2txt")
			  (specification->package "pandoc")
			  (specification->package "libreoffice")
			  
			  ;;; Utilities
			  (specification->package "curl"))
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
			;;	     (linux-arguments '("root=/dev/sdb1"))
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
