(use-modules (gnu home)
	     (gnu home services)
	     (gnu home services shells)
	     (gnu services)
	     (gnu packages)
	     (gnu packages admin)
	     (guix gexp))

(home-environment
 (packages (append (map specification->package '(
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
				     "audacity"

				     ;; Reading
				     "calibre"

				     ;; Research


				     ;; Browsers
				     "nyxt"

				     ;; Common Lisp
				     "cl-anaphora"
				     "cl-alexandria"
				     "cl-asdf"

				     ;; Messaging
				     "gnunet"
				     "qtox"
				     "telegram-desktop"

				     ;; Torrenting
				     "qbittorrent"
				     "rtorrent"

				     ;; Astrophysics
				     "stellarium"

				     ;; Utilities
				     "clipmenu"
				     "qdirstat"

				     )))))
