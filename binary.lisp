;;; binary.lisp
(in-package #:cl-frame)

(defstruct BigEndian ())

(defmethod put-uint16 ((obj BigEndian) buf uint16)
  (declare (type (array (unsigned-byte 8) (2)) buf))
  (let* ((octs (bit-smasher:octets<- uint16))
         (buf-last-idx (- (length buf) 1))
         (octs-last-idx (- (length octs) 1))
         )

	(loop for i from 0 below (length octs)
          do
             (setf (aref buf (- buf-last-idx i))
                   (aref octs (- octs-last-idx i)))
          )
    )
  )

(defmethod put-uint32 ((obj BigEndian) buf uint32)
  (declare (type (array (unsigned-byte 8) (4)) buf))
  (let* ((octs (bit-smasher:octets<- uint32))
         (buf-last-idx (- (length buf) 1))
         (octs-last-idx (- (length octs) 1))
         )

	(loop for i from 0 below (length octs)
          do
             (setf (aref buf (- buf-last-idx i))
                   (aref octs (- octs-last-idx i)))
          )
    )
  )


(defmethod put-uint24 ((obj BigEndian) buf uint24)
  (declare (type (array (unsigned-byte 8) (6)) buf))
  (let* ((octs (bit-smasher:octets<- uint24))
         (buf-last-idx (- (length buf) 1))
         (octs-last-idx (- (length octs) 1))
         )

	(loop for i from 0 below (length octs)
          do
             (setf (aref buf (- buf-last-idx i))
                   (aref octs (- octs-last-idx i)))
          )
    )
  )

(defmethod put-uint64 ((obj BigEndian) buf uint64)
  (declare (type (array (unsigned-byte 8) (8)) buf))
  (let* ((octs (bit-smasher:octets<- uint64))
         (buf-last-idx (- (length buf) 1))
         (octs-last-idx (- (length octs) 1))
         )

	(loop for i from 0 below (length octs)
          do
             (setf (aref buf (- buf-last-idx i))
                   (aref octs (- octs-last-idx i)))
          )
    )
  )

(defstruct LittlEndian ())


(defmethod put-uint16 ((obj LittlEndian) buf uint16)
  (declare (type (array (unsigned-byte 8) (2)) buf))
  (let* ((octs (bit-smasher:octets<- uint16)))

	(loop for i from 0 below (length octs)
          do (setf (aref buf i) (aref octs i)))
    )
  )

(defmethod put-uint32 ((obj LittlEndian) buf uint32)
  (declare (type (array (unsigned-byte 8) (4)) buf))
  (let* ((octs (bit-smasher:octets<- uint32)))

	(loop for i from 0 below (length octs)
          do (setf (aref buf i) (aref octs i)))
    )
  )


(defmethod put-uint24 ((obj LittlEndian) buf uint24)
  (declare (type (array (unsigned-byte 8) (6)) buf))
  (let* ((octs (bit-smasher:octets<- uint24)))

	(loop for i from 0 below (length octs)
          do (setf (aref buf i) (aref octs i)))
    )
  )

(defmethod put-uint64 ((obj LittlEndian) buf uint64)
  (declare (type (array (unsigned-byte 8) (8)) buf))
  (let* ((octs (bit-smasher:octets<- uint64)))

	(loop for i from 0 below (length octs)
          do (setf (aref buf i) (aref octs i)))
    )
  )
