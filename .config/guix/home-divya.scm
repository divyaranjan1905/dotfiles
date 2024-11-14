;;; Divya's home profile on Guix system.
;;; This lives in ~/.config/guix/home-divya.scm and initiated through guix home reconfigure

(define-module (guix-home-config)
  #:use-module (gnu home)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu system shadow))


;;; Services

;; Emacs Daemon Shepherd Service

;; (define (emacs-daemon-shepherd-service config)
;;   (list (shepherd-service
;; 	 (documentation "Emacs server.")
;; 	 (provision '(emacs-server))
;; 	 (start #~(make-forkec-constructor
;; 		   (list #$ (file-append emacs "/bin/emacs")
;; 			 "--fg-daemon")))
;; 	 (stop #~(make-kill-destructor)))))

;; (define home-emacs-daemon-service-type
;;   (service-type
;;    (name 'home-emacs-daemon)
;;    (extensions (list (service-extension home-shepherd-service-type
;; 					emacs-daemon-shepherd-service)))
;;    (default-value #f)
;;    (description "Emacs daemon as a Shepherd service.")))

(home-environment
 (packages (append (map specification->package
			'(
			  ;; Themes
			  "materia-theme"
			  "arc-icon-theme"
			  "papirus-icon-theme"
			  "bibata-cursor-theme"

			  ;; Fonts
			  "font-google-noto-emoji"

			  ;; Audio
			  "ardour"
			  "mumble"
			  "pipewire"
			  "wireplumber"
			  "qpwgraph"
			  "pavucontrol"
			  "carla"
			  ;; "audacity"

			  ;; Reading
			  "calibre"
			  "djvulibre"
			  "djvu2pdf"

			  ;; Research
			  "anki"

			  ;; Emacs
			  "emacs-pdf-tools"
			  "emacs-saveplace-pdf-view"

			  ;; Essential R
			  "r-dplyr"
			  "r-tidyverse"
			  "r-magrittr"
			  "r-ggplot2"

			  ;; YouTube
			  "ytfzf"

			  ;; LaTeX
			  "texlive-biber"

			  ;; Browsers
			  ;; "nyxt"

			  ;; Compression
			  "unzip"

			  ;; Common Lisp
			  "cl-anaphora"
			  "cl-alexandria"
			  "cl-asdf"

			  ;; Spelling
			  "ispell"

			  ;; Printing
			  "cups"
			  "hplip"
			  "hplip-minimal"

			  ;; Messaging
			  "gnunet"
			  ;; "qtox"
			  "jami"
			  "telegram-desktop"

			  ;; Torrenting
			  "qbittorrent"
			  "rtorrent"

			  ;; Astrophysics
			  ;; "stellarium"

			  ;; Security
			  "gnupg" ; Installed here and not in system.scm because the service is in home
			  "pinentry"
			  "pinentry-emacs"
			  "pinentry-tty"

			  ;; Syncing
			  "syncthing"
			  "syncthing-gtk"

			  ;; Utilities
			  "clipmenu"
			  "qdirstat"

			  ;; Board
			  "xournalpp"

			  ))))

 (services (append
	    (list
	     (service home-gpg-agent-service-type
                      (home-gpg-agent-configuration
                       (pinentry-program
			(file-append pinentry "/bin/pinentry"))
                       (ssh-support? #t)
                       (default-cache-ttl 3000)
                       (max-cache-ttl 6000)
                       (extra-content "\nallow-loopback-pinentry\nallow-emacs-pinentry\n")))

	     (service home-syncthing-service-type)
	     (service home-dbus-service-type)
	     (service home-pipewire-service-type
		      (home-pipewire-configuration
		       (wireplumber wireplumber)
		       (enable-pulseaudio? #t)))))))
