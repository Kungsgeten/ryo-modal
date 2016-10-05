;;; ryo-modal.el --- Roll your own modal mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/ryo-modal
;; Keywords: convenience, modal, keys
;; Version: 0.1
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

(defvar ryo-modal-global-mode-blacklist ()
  "Modes excluded from `ryo-modal-global-mode'.")

(defvar ryo-modal-bindings-list ()
  "A list of all the bindings in `ryo-modal-mode'.")

;;;###autoload
(defun ryo-modal-key (key target &rest args)
  "Bind KEY to TARGET (command or another keybinding) in `ryo-modal-mode'.
ARGS should be of the form [:keyword option]...

:name    Can be a string, naming the binding.  If ommited get name from TARGET.
:exit    If t then exit `ryo-modal-mode' after the command.
:read    If t then insert a string after the command.
:mode    If set to a major mode symbol (e.g. 'org-mode) the key will only be
         bound in that mode.
:then    Can be a quoted list of additional commands that will be run after
         the first.  These will not be shown in the name of the binding.
         (use :name to give it a nickname)."
  (let* ((name (or (plist-get args :name)
                   (if (stringp target)
                       target
                     (symbol-name target))))
         (func
          (defalias (make-symbol (concat "ryo:" name))
            (lambda ()
              (interactive)
              (call-interactively (if (stringp target)
                                      (key-binding (kbd target))
                                    target))
              (mapc #'call-interactively (plist-get args :then))
              (when (plist-get args :exit) (ryo-modal-mode -1))
              (when (plist-get args :read) (insert (read-string "Insert: "))))
            (if (stringp target)
                (format "%s → %s (`%s')\n\n%s%s"
                        (key-description (kbd key))
                        (key-description (kbd target))
                        (key-binding (kbd target))
                        (documentation (key-binding (kbd target)))
                        (mapconcat #'documentation (plist-get args :then) "\n"))
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
    (add-to-list 'ryo-modal-bindings-list `(,key ,name ,@args))))

;;;###autoload
(defmacro ryo-modal-keys (&rest args)
  "Bind several keys in `ryo-modal-mode'.
Each element in ARGS should of the form (key target [keywords]).
The target should not be quoted.
See `ryo-modal-key' for more information."
  `(progn
     ,@(mapcar (lambda (x)
                 `(ryo-modal-key ,(car x)
                                 (if ,(stringp (cadr x))
                                     ,(cadr x)
                                   (quote ,(cadr x)))
                                 ,@(nthcdr 2 x)))
               args)))

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
          (add-hook 'post-command-hook #'ryo-modal--cursor-color-update)          )
        (setq-local cursor-type ryo-modal-cursor-type)
        (let ((map (eval (intern-soft (concat "ryo-" (symbol-name major-mode) "-map")))))
          (when map
            (make-local-variable 'minor-mode-overriding-map-alist)
            (push `(ryo-modal-mode . ,map) minor-mode-overriding-map-alist))))
    (remove-hook 'post-command-hook #'ryo-modal--cursor-color-update)
    (set-cursor-color ryo-modal-default-cursor-color)
    (setq-local cursor-type (default-value 'cursor-type))))

(defun ryo-modal--global-on-p ()
  "If `ryo-modal-global-mode' should activate in a new buffer or not."
  (unless (or (minibufferp)
              (member major-mode ryo-modal-global-mode-blacklist))
    (ryo-modal-mode 1)))

(define-globalized-minor-mode ryo-modal-global-mode
  ryo-modal-mode
  ryo-modal--global-on-p)

(defun ryo-modal--cursor-color-update ()
  "Set cursor color depending on if `ryo-modal-mode' is active or not."
  (if ryo-modal-mode
      (set-cursor-color ryo-modal-cursor-color)
    (set-cursor-color ryo-modal-default-cursor-color)))

(provide 'ryo-modal)
;;; ryo-modal.el ends here
