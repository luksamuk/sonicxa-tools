(in-package #:sonicxa-tools.utils)

(defun check-endianness ()
  (let* ((num #x01020304)
         (first-byte (ldb (byte 8 0) num)))
    (cond
      ((= first-byte #x04) :little-endian)
      ((= first-byte #x01) :big-endian)
      (t (error "Could not check endianness")))))

(defun byte-at (value bitshift)
  (coerce (mod (ash value bitshift) 256) '(unsigned-byte 8)))

(defun value-bytes-msb-to-lsb (value unit) ; inverted from this
  (loop for i from (- unit 8) downto 0 by 8
        collect (byte-at value (- i))))

(defun value-bytes-lsb-to-msb (value unit) ; same as this
  (loop for i from 0 upto (- unit 8) by 8
        collect (byte-at value (- i))))

(defun value-bytes-be (value unit)
  (if (eq (check-endianness) :little-endian)
      (value-bytes-msb-to-lsb value unit)
      (value-bytes-lsb-to-msb value unit)))

(defun write-bytes-be (value unit stream)
  (let ((bytes (value-bytes-be value unit)))
    (dolist (byte bytes)
      (write-byte byte stream))))

(defun write-u8 (stream value)
  (write-bytes-be value 8 stream))

(defun write-u16 (stream value)
  (write-bytes-be value 16 stream))

(defun write-u32 (stream value)
  (write-bytes-be value 32 stream))

(defun write-u64 (stream value)
  (write-bytes-be value 64 stream))


;; Convert stuff to/from fixed point with 12-scale
(defun float->fixed (value scale)
  (declare (type number value)
           (type fixnum scale))
  (multiple-value-bind (v)
      (coerce (truncate (* value (ash 1 scale))) 'fixnum)
    v))

(defun float->fixed12 (value)
  (declare (type number value))
  (float->fixed value 12))
