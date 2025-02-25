(asdf:defsystem #:sonicxa-tools
  :description "Sonic XA Developer Tools"
  :author "Lucas S. Vieira"
  :license "MPL-2.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:uiop
               #:alexandria
               #:cl-json
	       #:cxml
	       #:cl-toml
               #:data-frame
               #:cl-geometry)
  :components
  ((:file "package")
   (:module "src"
    :components
	    ((:file "utils")
             (:file "parallax")
             (:file "main")))))
