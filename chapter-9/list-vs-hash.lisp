(load "chapter-8/grand-theft-wumpus.lisp")

(setf *edge-num* 1000)
(setf *node-num* 1000)

(time (dotimes (i 100) (get-connected 1 (make-edge-list))))

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
	    (let ((node (car x)))
	      (push (cdr x) (gethash node tab))))
	  edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
		       (unless (gethash node visited)
			 (setf (gethash node visited) t)
			 (mapc (lambda (edge)
				 (traverse edge))
			       (gethash node edge-tab)))))
      (traverse node))
    visited))

(time (dotimes (i 100)
	(get-connected-hash 1 (hash-edges (make-edge-list)))))
