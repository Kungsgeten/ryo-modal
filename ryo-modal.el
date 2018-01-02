;;; ryo-modal.el --- Roll your own modal mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/ryo-modal
;; Keywords: convenience, modal, keys
;; Version: 0.3
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; ryo-modal provides a convenient way of defining modal keybindings in Emacs.
;; The primary way of binding keys is using `ryo-modal-key' and `ryo-modal-keys'.
;; Both of these functions provide useful keyword arguments.
;; `ryo-modal-mode' is used to toggle the modal editing environment.
;; ryo-modal does not come with any predefined bindings!
;;
;; If you want bindings that only should be active when the region is active,
;; please have a look at `selected-minor-mode' (https://github.com/Kungsgeten/selected.el)

;;; Code:
(require 'cl-lib)

(defvar ryo-modal-mode-map (make-sparse-keymap)
  "General bindings in ryo-modal-mode.
Major mode specific bindings will be bound to ryo-<major-mode>-map instead.")

(defvar ryo-modal-cursor-type t
  "Cursor type used in `ryo-modal-mode'.  See description of `cursor-type'.")

(defvar ryo-modal-cursor-color "red"
  "The cursor color used in `ryo-modal-mode'.  If nil then use default color.")

(defconst ryo-modal-default-cursor-color (face-attribute 'cursor :background)
  "Default color of cursor.")

(defvar ryo-modal-bindings-list ()
  "A list of all the bindings in `ryo-modal-mode'.
It is more convenient to view this using `ryo-modal-bindings'.")

(defvar ryo-modal--last-command nil)

(defun ryo-modal-repeat ()
  "Repeat last ryo command.

Because of how `ryo-modal-key' works, normal `repeat' don't play
well with `ryo-modal-mode'. Use this function instead, to repeat
ryo commands.  Do not bind it using `ryo-modal-key', instead use
standard `ryo-modal-mode-map':

  (define-key ryo-modal-mode-map (kbd \",\") 'ryo-modal-repeat)
  (add-to-list 'ryo-modal-bindings-list '(\",\" \"ryo-modal-repeat\"))

If you do not want a command to be remembered by `ryo-modal-repeat',
add :norepeat t as a keyword."
  (interactive)
  (when ryo-modal--last-command
    (command-execute ryo-modal--last-command nil nil t)))

;;;###autoload
(defun ryo-modal-key (key target &rest args)
  "Bind KEY to TARGET in `ryo-modal-mode'.

TARGET can be one of:

kbd-string   Pressing KEY will simulate TARGET as a keypress.
command      Calls TARGET interactively.
list         Each element of TARGET is sent to `ryo-modal-key' again, with
             KEY as a prefix key.
:hydra       If you have hydra installed, a new hydra will be created and
             bound to KEY.  The first element of ARGS should be a list
             containing the arguments sent to `defhydra'.

ARGS should be of the form [:keyword option]... if TARGET is a kbd-string
or a command.  The following keywords exist:

:name      Can be a string, naming the binding.  If ommited get name from TARGET.
:exit      If t then exit `ryo-modal-mode' after the command.
:read      If t then insert a string after the command.
:mode      If set to a major mode symbol (e.g. 'org-mode) the key will only be
           bound in that mode.
:then      Can be a quoted list of additional commands that will be run after
           the first.  These will not be shown in the name of the binding.
           (use :name to give it a nickname).
:norepeat  If t then do not become a target of `ryo-modal-repeat'."
  (cond
   ((listp target)
    (mapc (lambda (x)
            ;; Merge :then lists
            (when (and (plist-get (cddr x) :then)
                       (plist-get args :then))
              (setf (cddr x) (plist-put (cddr x) :then (append (plist-get (cddr x) :then)
                                                               (plist-get args :then)))))
            (apply #'ryo-modal-key `(,(concat key " " (car x))
                                     ,@(cdr x)
                                     ,@args)))
          target))
   ((and (require 'hydra nil t)
         (equal target :hydra))
    (apply #'ryo-modal-key `(,key ,(eval `(defhydra ,@(car args))) ,@(cdr args))))
   (t
    (let* ((name (or (plist-get args :name)
                     (if (stringp target)
                         target
                       (symbol-name target))))
           (func
            (defalias (make-symbol (concat "ryo:" name))
              (lambda ()
                (interactive)
                (let ((cmd-lambda
                       (lambda ()
                         (interactive)
                         ;; Exit if key bound to keymap key
                         (if (and (stringp target)
                                  (keymapp (key-binding (kbd target))))
                             (progn
                               (when (plist-get args :exit) (ryo-modal-mode -1))
                               (setq unread-command-events (listify-key-sequence (kbd target))))
                           (call-interactively (if (stringp target)
                                                   (key-binding (kbd target))
                                                 target))
                           (mapc #'call-interactively (plist-get args :then))
                           (when (plist-get args :exit) (ryo-modal-mode -1))
                           (when (plist-get args :read) (insert (read-string "Insert: ")))))))
                  (unless (plist-get args :norepeat)
                    (setq ryo-modal--last-command cmd-lambda))
                  (command-execute cmd-lambda nil nil t)))
              (if (stringp target)
                  (if (keymapp (key-binding (kbd target)))
                      (concat "Call keymap " target)
                    (format "%s → %s (`%s')\n\n%s%s"
                            (key-description (kbd key))
                            (key-description (kbd target))
                            (key-binding (kbd target))
                            (documentation (key-binding (kbd target)))
                            (mapconcat #'documentation (plist-get args :then) "\n")))
                (concat (documentation target)
                        (mapconcat #'documentation (plist-get args :then) "\n")))))
           (mode (plist-get args :mode)))
      (if mode
          (let ((map-name (format "ryo-%s-map" mode)))
            (unless (intern-soft map-name)
              (set (intern map-name) (make-sparse-keymap))
              (set-keymap-parent (eval (intern map-name))
                                 ryo-modal-mode-map))
            (define-key (eval (intern map-name)) (kbd key) func))
        (define-key ryo-modal-mode-map (kbd key) func))
      (add-to-list 'ryo-modal-bindings-list `(,key ,name ,@args))))))

;;;###autoload
(defmacro ryo-modal-keys (&rest args)
  "Bind several keys in `ryo-modal-mode'.
Typically each element in ARGS should be of the form (key target [keywords]).
The target should not be quoted.
The first argument may be a list of keywords; they're applied to all keys:

  \(:exit t :then '(kill-region)).

See `ryo-modal-key' for more information."
  (let ((kw-list
         (if (symbolp (caar args))
             (pop args)
           nil)))
    `(progn
       ,@(mapcar (lambda (x)
                   `(ryo-modal-key ,(car x)
                                   (if ,(stringp (cadr x))
                                       ,(cadr x)
                                     (quote ,(cadr x)))
                                   ,@(nthcdr 2 x)
                                   ,@kw-list))
                 args))))

;;;###autoload
(defmacro ryo-modal-major-mode-keys (mode &rest args)
  "Bind several keys in `ryo-modal-mode', but only if major mode is MODE.
See `ryo-modal-keys' for more information."
  `(progn
     ,@(mapcar (lambda (x)
                 `(ryo-modal-key ,(car x)
                                 (if ,(stringp (cadr x))
                                     ,(cadr x)
                                   (quote ,(cadr x)))
                                 ,@(nthcdr 2 x)
                                 :mode ,mode))
               args)))

;;;###autoload
(defun ryo-modal-set-key (key command)
  "Give KEY a binding as COMMAND in `ryo-modal-mode-map'.

This function is meant to be used interactively, if you want to
temporarily bind a key in ryo.

See `global-set-key' for more info."
  (interactive "KSet ryo key: \nCSet ryo key %s to command: ")
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key ryo-modal-mode-map key command))

;;;###autoload
(defun ryo-modal-unset-key (key)
  "Remove `ryo-modal-mode-map' binding of KEY.
KEY is a string or vector representing a sequence of keystrokes.

This function is meant to unbind keys set with `ryo-modal-set-key'."
  (interactive "kUnset ryo key: ")
  (define-key ryo-modal-mode-map key nil))

;;;###autoload
(defun ryo-modal-bindings ()
  "Display a buffer of all bindings in `ryo-modal-mode'."
  (interactive)
  (let ((key-column-width 18)
        (command-column-width 40))
    (cl-flet ((ryo-princ-bindings
               (bindings)
               (mapc (lambda (x)
                       (let ((keywords (nthcdr 2 x)))
                         (princ (concat (car x)
                                        (make-string (- key-column-width (length (car x))) ? )
                                        (cadr x)
                                        (if (plist-get keywords :exit) " → EXIT")
                                        (if (plist-get keywords :read) " → READ")
                                        "\n"))
                         (when (and (not (plist-get keywords :name))
                                    (plist-get keywords :then))
                           (princ (concat (make-string key-column-width ? )
                                          (mapconcat (lambda (x)
                                                       (concat " → " (symbol-name x)))
                                                     (plist-get keywords :then)
                                                     (concat "\n" (make-string key-column-width ? )))
                                          "\n")))))
                     bindings)))
      (with-output-to-temp-buffer "*ryo-modal-bindings*"
        (princ (format "Key%s Bound to\n%s %s\n"
                       (make-string (- key-column-width 4) ? )
                       (make-string (1- key-column-width) ?-)
                       (make-string (1- command-column-width) ?-)))
        (setq ryo-modal-bindings-list
              (sort ryo-modal-bindings-list
                    (lambda (l r)
                      (string< (car l) (car r)))))
        (ryo-princ-bindings (cl-remove-if (lambda (x)
                                            (plist-get (nthcdr 2 x) :mode))
                                          ryo-modal-bindings-list))
        (let ((modes))
          (mapc (lambda (x)
                  (add-to-list 'modes (plist-get (nthcdr 2 x) :mode)))
                ryo-modal-bindings-list)
          (mapc (lambda (x)
                  (when x
                    (princ (format "\n\n%s specific bindings\n%s\n"
                                   (symbol-name x)
                                   (make-string (+ key-column-width command-column-width) ?-)))
                    (ryo-princ-bindings (cl-remove-if-not (lambda (binding)
                                                            (equal x (plist-get (nthcdr 2 binding) :mode)))
                                                          ryo-modal-bindings-list))))
                modes))))))

;;;###autoload
(define-minor-mode ryo-modal-mode
  "Toggle `ryo-modal-mode'."
  nil " ryo" ryo-modal-mode-map
  (if ryo-modal-mode
      (progn
        (when ryo-modal-cursor-color
          (add-hook 'post-command-hook #'ryo-modal--cursor-color-update))
        (setq-local cursor-type ryo-modal-cursor-type)
        (let ((map (eval (intern-soft (concat "ryo-" (symbol-name major-mode) "-map")))))
          (when map
            (make-local-variable 'minor-mode-overriding-map-alist)
            (push `(ryo-modal-mode . ,map) minor-mode-overriding-map-alist))))
    (remove-hook 'post-command-hook #'ryo-modal--cursor-color-update)
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'ryo-modal-mode minor-mode-overriding-map-alist))
    (set-cursor-color ryo-modal-default-cursor-color)
    (setq-local cursor-type (default-value 'cursor-type))))

(defun ryo-modal--cursor-color-update ()
  "Set cursor color depending on if `ryo-modal-mode' is active or not."
  (if ryo-modal-mode
      (set-cursor-color ryo-modal-cursor-color)
    (set-cursor-color ryo-modal-default-cursor-color)))

;; use-package integration
(defun ryo-modal--extract-commands-from (args)
  "Extract commands from ARGS to enable lazy loading for :ryo."
  (let (commands)
    (dolist (arg args commands)
      (let ((target (cadr arg)))
        (cond
         ((listp target)
          (push (ryo-modal--extract-commands-from target) commands))
         ((equal target :hydra)
          (dolist (hydra-term (cadr (cdr arg)))
            (when (and hydra-term
                       (listp hydra-term)
                       (cadr hydra-term))
              (push (list (cadr hydra-term)) commands))))
         ((not (stringp target))
          (push (list target) commands)))))))

(with-eval-after-load 'use-package-core
  ;; step 1: introduce ryo-modal keyword before :bind
  (unless (member :ryo use-package-keywords)
    (setq use-package-keywords (use-package-list-insert :ryo use-package-keywords :bind)))

  ;; ensure deferred loading
  (when (boundp 'use-package-deferring-keywords)
    (add-to-list 'use-package-deferring-keywords :ryo t))

  ;; step 2: normalize
  (defun use-package-normalize/:ryo (_name _keyword args)
    "Apply lists of keywords to all keys following that list."
    (let (kwlist sanitized-args)
      (dolist (arg args sanitized-args)
        (cond
         ((symbolp (car arg))
          (setq kwlist arg))
         ((stringp (car arg))
          (push (append arg kwlist) sanitized-args))))))

  ;; step 3: handler
  (defun use-package-handler/:ryo (name _keyword arglists rest state)
    "Use-package handler for :ryo."

    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-append rest :commands
                                  (ryo-modal--extract-commands-from arglists)))
       state)
     `((ignore ,@(mapcar (lambda (arglist)
                           (if (stringp (cadr arglist))
                               `(ryo-modal-key ,(car arglist)
                                               ,(cadr arglist)
                                               ,@(nthcdr 2 arglist)
                                               :package ',name)
                             `(ryo-modal-key ,(car arglist)
                                             (quote ,(cadr arglist))
                                             ,@(nthcdr 2 arglist)
                                             :package ',name)))
                         arglists))))))

(provide 'ryo-modal)
;;; ryo-modal.el ends here
