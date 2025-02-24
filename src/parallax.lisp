(in-package #:buildprl)

(defstruct parallax-strip
  (name "" :type string)
  (u0 0 :type fixnum)
  (v0 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (bg-index 0 :type fixnum)
  (single nil :type fixnum)
  (scrollx 0 :type fixnum)
  (speedx 0 :type fixnum)
  (y0 0 :type fixnum))

(defmethod print-object ((obj parallax-strip) stream)
  (let ((fmt "~{~2,'0x ~}"))
    (format stream
            (concatenate 'string
                         "#<PARALLAX-STRIP ~s~%"
                         "    u0       " fmt "~%"
                         "    v0       " fmt "~%"
                         "    width    " fmt "~%"
                         "    height   " fmt "~%"
                         "    bg-index " fmt "~%"
                         "    single   " fmt "~%"
                         "    scrollx  " fmt "~%"
                         "    speedx   " fmt "~%"
                         "    y0       " fmt ">")
            (parallax-strip-name obj)
            (utils:value-bytes-be (parallax-strip-u0 obj) 8)
            (utils:value-bytes-be (parallax-strip-v0 obj) 8)
            (utils:value-bytes-be (parallax-strip-width obj) 16)
            (utils:value-bytes-be (parallax-strip-height obj) 16)
            (utils:value-bytes-be (parallax-strip-bg-index obj) 8)
            (utils:value-bytes-be (parallax-strip-single obj) 8)
            (utils:value-bytes-be (parallax-strip-scrollx obj) 32)
            (utils:value-bytes-be (parallax-strip-speedx obj) 32)
            (utils:value-bytes-be (parallax-strip-y0 obj) 16))))

(defun parse-values (name value)
  (setf value (case value
                (cl-toml:false #x0)
                (cl-toml:true  #x1)
                (otherwise value)))
  (setf value (case name
                ((:scrollx :speedx) (float->fixed12 value))
                (otherwise value)))
  (list name value))

(defun parse-parallax (file)
  (loop for part being the hash-values of file
          using (hash-key part-name)
        collect
        (let ((plist
                (loop for value being the hash-values of part
                        using (hash-key key)
                      nconcing
                      (let ((name (intern (string-upcase key) 'keyword)))
                        (parse-values name value)))))
          ;; Generate texture index and wrap v0 around the Y size
          (setf plist (nconc
                       plist
                       (list :name part-name
                             :bg-index
                             (the fixnum (truncate (/ (getf plist :v0) 256))))))
          (setf (getf plist :v0)
                (rem (getf plist :v0) 256))
          ;; Generate structures
          (eval `(make-parallax-strip ,@plist)))))

(defun write-parallax (parallax stream)
  (utils:write-u8 stream (coerce (length parallax) 'fixnum))
  (dolist (part parallax)
    (utils:write-u8 stream (parallax-strip-u0 part))
    (utils:write-u8 stream (parallax-strip-v0 part))
    (utils:write-u16 stream (parallax-strip-width part))
    (utils:write-u16 stream (parallax-strip-height part))
    (utils:write-u8 stream (parallax-strip-bg-index part))
    (utils:write-u8 stream (parallax-strip-single part))
    (utils:write-u32 stream (parallax-strip-scrollx part))
    (utils:write-u32 stream (parallax-strip-speedx part))
    (utils:write-u16 stream (parallax-strip-y0 part))))

(defun gen-parallax (input-path)
  (let ((file (cl-toml:parse (alexandria:read-file-into-string input-path))))
    (parse-parallax file)))

(defun build-parallax (input-path output-path)
  (let ((parallax (gen-parallax input-path)))
    (with-open-file (stream output-path
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (write-parallax parallax stream))))


