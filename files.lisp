;;;; Portable directory and file utility functions.

;;; Based on chapter 15 of Practical Common Lisp.
;;; Compatible with SBCL, CMULisp, LispWorks, OpenMCL, Allegro and CLisp.
;;;
;;; Directory listing:
;;;   (list-directory dirname)
;;;
;;; Predicate for file existence:
;;;   (file-exists-p filename)
;;;
;;; Walking a directory tree:
;;;   (walk-directory dirname function [:directories] [:test test-f])

;; Directory listing

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

;; Probing for file existence

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

#+clisp
(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))

;; Walking a directory tree

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Given a directory name and a function, applies the function on each pathname
   encountered while walking the directory tree."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

;; Helper functions

(defun component-present-p (value)
  "Tests whether a given component of a pathname is present."
  (and value ; is not NIL
       (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  "Tests whether a pathname is already in directory form."
  (and
   p
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))))

(defun pathname-as-directory (name)
  "Converts any pathname into a 'directory form' pathname."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name      nil
	 :type      nil
	 :defaults pathname)
	pathname)))

(defun directory-wildcard (dirname)
  ;; Workaround for CLisp handling of DIRECTORY wildcard type component.
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))
