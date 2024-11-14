;;; Channels

(cons*
 ;; Guix Science
 (channel
  (name 'guix-science)
  (url "https://github.com/guix-science/guix-science.git")
  (branch "master")
  (introduction
   (make-channel-introduction
    "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
    (openpgp-fingerprint
     "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))

 ;; Rust-Next
 (channel
  (name 'rust-next)
  (url "https://github.com/umanwizard/guix-rust-next")
  (branch "main")
  (introduction
   (make-channel-introduction
    "72e021c9a90f9f417bdffca8799c8d3e0aa98a72"
    (openpgp-fingerprint
     "9E53FC33B8328C745E7B31F70226C10D7877B741"))))

 ;; Nonguix
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  ;; Enable signature verification:
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 %default-channels)
