(use-modules (gnu home)
	     (gnu home services)
	     (gnu home services shells)
	     (gnu services)
	     (gnu packages admin)
	     (guix gexp))

(home-environment
 (packages (specification->packages (list
				     "ardour"
				     "mumble"
				     "audacity"
				     "calibre"
				     "nyxt"
				     "obs-studio"
				     "gnunet"
				     "qtox"
				     "qbittorrent"
				     "rtorrent"
				     "stellarium"
				     "telegram-desktop"))))
