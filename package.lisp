(defpackage #:sonicxa-tools.main
  (:use #:cl))

;;; Potential packages:

;; Tiled exporter replacements
;; ===========================
;; - chunk-exporter
;;   Exports 128x128 chunks (.tmx) to *.cnk
;; - lvl-exporter
;;   Exports level maps (.tmx) to *.psxlvl

;; Other tooling
;; =============
;; - framepacker
;;   Converts exported *.json (from Aseprite) to *.chara
;;   Also converts 16x16 tiles (Tiled-exported to *.json) to MAP16.MAP
;; - chunkgen
;;   Converts *.cnk to MAP128.MAP
;; - cooklvl
;;   Converts *.psxlvl to *.LVL
;; - cookcollision
;;   Converts 16x16 tiles (Tiled-exported to *.json) to MAP16.COL
;; - buildprl
;;   Converts parallax.toml to PRL.PRL
;; - convrsd
;;   Converts legacy models (*.rsd + *.ply + *.mat) to *.mdl

;; New tools (see note below)
;; ==========================
;; - piecegen.tiles
;;   Converts 16x16 tilemap (*.tmx) to MAP16.MAP
;; - piecegen.collision
;;   Converts 16x16 tilemap (*.tmx) to MAP16.COL
;; - piecegen
;;   Amalgamates the previous ones into a single exporter.
;; - framepack
;;   Converts exported *.json (from Aseprite) to *.CHARA
;; - util
;;   General tooling (e.g. manipulating filenames and outputs, etc)

;; IMPORTANT
;; =========
;; 1. Since sprites have their own importance, we need to separate framepacker
;;    sprite export functionality from Tiled's 16x16 export.
;; 2. cookcollision will be joined into this new tool.
;; 3. chunkgen should follow the same principles of piecegen.tiles.
;; 4. cooklvl should follow the same principles of piecegen.tiles.
;; 5. convrsd should be rebuilt, we're no longer interested in legacy model
;;    formats; just convert Blender models and rigging to a proper new format.


(defpackage #:sonicxa-tools.utils
  (:use #:cl)
  (:nicknames :utils)
  (:documentation "Utilities for general work across tools")
  (:export value-bytes-be
           write-u8
           write-u16
           write-u32
           write-u64
           float->fixed
           float->fixed12))

(defpackage #:piecegen.tiles
  (:use #:cl #:sonicxa-tools.utils)
  (:documentation "Convert 16x16 tile mappings (*.tmx) to MAP16.MAP"))

(defpackage #:piecegen.collision
  (:use #:cl #:sonicxa-tools.utils)
  (:documentation "Convert 16x16 tile collision (*.tmx) to MAP16.COL"))

(defpackage #:piecegen
  (:use #:cl #:sonicxa-tools.utils)
  (:documentation "Convert 16x16 tiles (*.tmx) to MAP16.MAP and MAP16.COL"))

(defpackage #:framepack
  (:use #:cl #:sonicxa-tools.utils)
  (:documentation "Converts Aseprite 8x8 sprite frame maps (*.json) to *.CHARA"))

(defpackage #:chunkgen
  (:use #:cl #:sonicxa-tools.utils)
  (:documentation "Converts 128x128 tile mappings (*.tmx) to MAP128.MAP"))

(defpackage #:cooklvl
  (:use #:cl #:sonicxa-tools.utils)
  (:documentation "Converts level chunk mappings (*.tmx) to *.LVL"))

(defpackage #:buildprl
  (:use #:cl #:sonicxa-tools.utils)
  (:documentation "Converts level parallax.toml to PRL.PRL"))

;; TODO: cookmdl
;; (defpackage #:convrsd
;;   (:use #:cl)
;;   (:documentation "Converts model files (*.rsd and others) to *.MDL"))

;; TODO: Maybe some level viewer tools, etc?

