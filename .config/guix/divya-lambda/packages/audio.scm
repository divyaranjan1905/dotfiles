;;; Divya’s Guix audio or sound related packages

(define-module (divya-lambda packages audio)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)

  #:use-module (guix build utils)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public jack-mixer
  (package
    (name "jack-mixer")
    (version "19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jack-mixer/jack_mixer")
             (commit (string-append "release-" version))))
       (sha256
        (base32 "18m6a9asbwaslw418i2w04kgc6jgdpw01i3kawdqy903kw66hnhj"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:build-type "release"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'path-patch
                     (lambda _
                       (substitute* '("meson.build")
                         (("'/', 'etc', 'xdg'")
                          (string-append "'"
                                         #$output "'")))))
                   (add-after 'install 'wrap-path
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (bin (string-append out "/bin/"))
			      (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                              (version #$(version-major+minor (package-version
                                                               (this-package-input
                                                                "python"))))
                              (lib (string-append out "/lib/python" version
                                                  "/site-packages")))
                         (wrap-program (string-append bin "jack_mixer")
                           `("GUIX_PYTHONPATH" ":" prefix
                             (,(getenv "GUIX_PYTHONPATH") ,lib))
			   `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (native-inputs (list pkg-config
                         python-cython
                         python-docutils
                         gettext-minimal
                         glib))
    (inputs (list bash-minimal))
    (propagated-inputs (list gtk+
			     `(,gtk+ "bin")
			     python
			     python-wrapper
			     python-pygobject
			     python-pycairo
			     python-platformdirs
			     jack-2))
    (synopsis
     "JACK Mixer: A multi-channel audio mixer for the JACK Audio Connection Kit")
    (description
     "The jack_mixer is a GTK+ JACK audio mixer app with a look & handling
similar to hardware mixing desks.  It has lot of useful features, apart
from being able to mix multiple JACK audio streams.")
    (home-page "https://rdio.space/jackmixer/")
    (license license:gpl2+)))
