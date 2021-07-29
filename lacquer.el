;;; lacquer.el --- Package description (Theme switch and save)  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 dingansich_kum0

;; Author: dingansich_kum0 <zy.hua1122@outlook.com>
;; URL: https://example.com/package-name.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: theme switch

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Installation

;; put this file in your load-path.

;;;;; MELPA

;;;;; Manual

;; Install these required packages:

;; + cl-lib
;; + ivy

;; put this in your init.
;; file:

;; (require 'lacquer)
;; (lacquer-mode)

;;;; Usage

;; Run one of these commands:

;; `lacquer-mode': start up lacquer-mode.

;;;; Tips

;;  (use-package lacquer
;;    :ensure nil
;;    :load-path "~/.emacs.d/site-lisp/lacquer"
;;    :hook
;;    (after-init . lacquer-mode)
;;    :custom
;;    (lacquer/theme-list '((monokai-theme monokai)
;; (monokai-pro-theme monokai-pro)
;; (dracula-theme dracula (setq xxx xxx))
;; (doom-themes doom-one-light)
;; (doom-themes doom-vibrant)
;; (doom-themes doom-nord))))

;;;; Credits

;; This package would not have been possible without the following
;; packages: cl-lib, support for cl utils, and ivy, support for ivy-read.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'ivy)

;;;; Customization

(defgroup lacquer nil
  "Settings for `lacquer'."
  ;; :prefix "lacquer-"
  :group 'utils)


(defcustom lacquer/theme-list '((monokai-theme monokai))
  "Theme list.
E.g: '((theme-package-name theme-name config)).
Required: theme-package-name theme-name.
Optional: config.Any function."
  :type 'lisp)


(defcustom lacquer/default-theme 'monokai
  "Default theme name."
  :type 'symbol)


(defcustom lacquer/theme-cache "~/.emacs.d/.theme"
  "Path of theme cache."
  :type 'string)


(defcustom lacquer/prefix-key "C-c T"
  "Trigger of prefix key."
  :type 'string)


(defun lacquer-generate-keys-index-list (&optional prefix)
  "Generate keys index.
PREFIX is optional string."
  (let ((func 'number-to-string))
    (when (stringp prefix)
      (setq func
            (lambda (i)
              (concat prefix (number-to-string i)))))
    (mapcar func (number-sequence 1 9))))


(defcustom lacquer/keys-map-index
  (append (lacquer-generate-keys-index-list)
          '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y")
          (lacquer-generate-keys-index-list "z"))
  "Keys map."
  :type 'list)


(defcustom lacquer/selector-key "C-c T S"
  "Selector bind key."
  :type 'string)


(defcustom lacquer/current-theme-key "C-c T 0"
  "Current theme bind key."
  :type 'string)

;;;; Variables

(defvar lacquer/started nil
  "Lacquer mode has started.")


(defvar lacquer/current-theme nil
  "Current theme name.")


(defvar lacquer/theme-name-list (mapcar #'(lambda (i) (nth 1 i)) lacquer/theme-list)
  "Theme name list.")

;;;;; Private

(defun lacquer-set-current-theme ()
  "Set current theme."
  (let ((theme
         (lacquer-read-cache)))
    (setq lacquer/current-theme
          (if (lacquer-theme-is-existing theme) theme lacquer/default-theme))
    ))


(defun lacquer-write-cache (theme)
  "Write theme name to cache.
THEME: theme symbol name."
  (write-region (symbol-name theme) nil lacquer/theme-cache))


(defun lacquer-read-cache ()
  "Read theme symbol from cache."
  (when (file-exists-p lacquer/theme-cache)
    (intern
     (with-temp-buffer (insert-file-contents lacquer/theme-cache) (buffer-string)))
    ))


(defun lacquer-write-defualt-theme ()
  "Write default theme."
  (lacquer-write-cache lacquer/default-theme))


(defun lacquer-theme-is-existing (name)
  "Check the theme is exists.
NAME is theme symbol mame."
  (cl-loop for i in lacquer/theme-list
           when (eq (nth 1 i) name)
           return t))


(defun lacquer-read-theme ()
  "Read theme from cache."
  (let ((theme (lacquer-read-cache)))
    (if (lacquer-theme-is-existing theme)
        theme
      (lacquer-write-defualt-theme) lacquer/default-theme)
    ))


(defmacro lacquer-factory-macro (name load-name &rest config)
  "Factory macro.
NAME is theme package name.
LOAD-NAME is theme name.
CONFIG is theme config."
  `(progn
     (unless (package-installed-p (quote ,name))
       (package-install (quote ,name)))
     (when (symbolp (quote ,load-name))
       (disable-theme lacquer/current-theme)
       (load-theme (quote ,load-name) t)
       ,@config
       
       (setq lacquer/current-theme  (quote ,load-name))
       (lacquer-write-cache (quote ,load-name))
       (message (format "<%s> loaded successfully."
                        (symbol-name (quote ,load-name)))))
     ))


(defun lacquer-func-factory (theme)
  "Interactive function factory.
THEME is a list.  e.g: '(theme-package-name theme-name config)."
  `(defun ,(nth 1 theme) ()
     "Theme."
     (interactive)
     (unless (eq (quote ,(nth 1 theme)) lacquer/current-theme)
       (lacquer-factory-macro ,@theme)
       )))


(defmacro lacquer-func-macro-factory ()
  "Theme function macro factory."
  `(progn ,@(mapcar 'lacquer-func-factory lacquer/theme-list)))

;;;;; Public

;;;###autoload
(defun lacquer-current-theme ()
  "Current theme."
  (interactive)
  (message "Current theme: <%s>." (symbol-name lacquer/current-theme)))

(cl-loop with i = 0
         for v in lacquer/theme-list
         if (eq (nth 1 v) lacquer/current-theme)
         return i
         else
         do (incf i)
         finally return i)


;;;###autoload
(defun lacquer-selector ()
  "Theme selector."
  (interactive)
  (let* ((selected (cl-loop with i = 0
                            for v in lacquer/theme-list
                            if (eq (nth 1 v) lacquer/current-theme)
                            return i
                            else
                            do (incf i)
                            finally return i))
         (func-str (ivy-read (format "Current theme is <%s>. Please choose: " (symbol-name lacquer/current-theme))
                             lacquer/theme-name-list
                             :sort nil
                             :require-match t
                             :preselect selected
                             ))
         (func (intern func-str)))
    (if (fboundp func)
        (funcall func)
      (message (format "<%s> is no existing." func-str)))
    ))

;;;;; Keymaps

(defun lacquer-generate-map (map)
  "Generate key map.
MAP is keymap."
  (cl-loop with i = 0
           for v in lacquer/theme-list
           do (progn
                (define-key map (kbd (concat lacquer/prefix-key " " (nth i lacquer/keys-map-index)))  (nth 1 v))
                (incf i))
           finally return map
           ))


(defvar lacquer-mode-map (make-sparse-keymap "lacquer map")
  "Lacquer keymap.")


(define-key lacquer-mode-map (kbd lacquer/current-theme-key) 'lacquer-current-theme)
(define-key lacquer-mode-map (kbd lacquer/selector-key) 'lacquer-selector)

;; Minor-mode

(defun lacquer-start-up ()
  "Start up."
  (lacquer-func-macro-factory)
  (lacquer-generate-map lacquer-mode-map)
  (funcall (lacquer-read-theme))
  (setq lacquer/started t))


;;;###autoload
(define-minor-mode lacquer-mode
  "Minor mode to enable lacquer."
  :init-value nil
  :group 'lacquer
  :keymap lacquer-mode-map
  :global t
  :lighter nil
  (unless lacquer/started
    (lacquer-start-up)))


(provide 'lacquer)
;;; lacquer.el ends here
