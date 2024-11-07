;;; Divya’s Guix audio or sound related packages

(define-module (divya-packages-audio)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system meson)
  #:use-module (guix licenses)
  #:use-module (guix gexp)

  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk))

(define-public jack-mixer
  (package
    (name "jack-mixer")
    (version "19")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://github.com/jack-mixer/jack_mixer/releases/download/release-"
				  version
				  "/jack_mixer-"
				  version
				  ".tar.xz"))
	      (sha256
	       (base32
		"059v4r5avh67p402kwr47vb2k5hl3fpj4kfkk75b0mhry36by338"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:build-type "release"
      #:phases #~(modify-phases %standard-phases
		   (add-after 'unpack 'path-patch
		     (lambda _
		       (substitute* '("meson.build")
			 (("'/', 'etc', 'xdg'")
			  (string-append "'" #$output "'"))))))
      ))
    (native-inputs
     (list gcc-toolchain
	   pkg-config
	   python
	   python-cython
	   python-docutils
	   gnu-gettext
	   glib
	   gtk+
	   `(,gtk+ "bin")
	   ))
    (inputs
     (list python
	   python-pygobject
	   python-pycairo
	   python-platformdirs
	  jack-2))
   (synopsis "JACK Mixer: A multi-channel audio mixer desktop application for the JACK Audio Connection Kit.")
   (description
    "jack_mixer is a GTK+ JACK audio mixer app with a look & handling similar to hardware mixing desks. It has lot of useful features, apart from being able to mix multiple JACK audio streams.")
   (home-page "https://rdio.space/jackmixer/")
   (license gpl2+)))

jack-mixer
