;;; lacquer.el --- Package description (a util that switches theme and to configure cache)  -*- lexical-binding: t; -*-

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
(require 'utils)
(require 'setting)

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
  :type 'list)


(defcustom lacquer/default-theme 'monokai
  "Default theme."
  :type 'symbol)


(defcustom lacquer/font-list '(Menlo Fira\ Code)
  "Font list.
E.g: '((theme-package-name theme-name config)).
Required: theme-package-name theme-name.
Optional: config.Any function."
  :type 'lisp)


(defcustom lacquer/default-font 'Menlo
  "Default font."
  :type 'symbol)


(defcustom lacquer/default-font-size 135
  "Default font size."
  :type 'integer)


(defcustom lacquer/cache "~/.emacs.d/.lacquer"
  "Path of cache."
  :type 'string)


(defcustom lacquer/theme-prefix-key "C-c T"
  "Trigger theme of prefix key."
  :type 'string)


(defcustom lacquer/font-prefix-key "C-c F"
  "Trigger of prefix key."
  :type 'string)


(defcustom lacquer/keys-map-index
  (append (lacquer-generate-keys-index-list)
          '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y")
          (lacquer-generate-keys-index-list "z"))
  "Keys map."
  :type 'list)


(defcustom lacquer/theme-selector-key "C-c T S"
  "Theme selector bind key."
  :type 'string)


(defcustom lacquer/font-selector-key "C-c F S"
  "Font selector bind key."
  :type 'string)


(defcustom lacquer/font-increase-key "C-c F +"
  "Font increase bind key."
  :type 'string)


(defcustom lacquer/font-decrease-key "C-c F _"
  "Font increase bind key."
  :type 'string)


(defcustom lacquer/current-theme-key "C-c T 0"
  "Current theme bind key."
  :type 'string)


(defcustom lacquer/current-font-key "C-c F 0"
  "Current theme bind key."
  :type 'string)


(defcustom lacquer/font-size-step 5
  "Current theme bind key."
  :type 'integer)

;;;; Variables

(defvar lacquer/started nil
  "Lacquer mode has started.")


(defvar lacquer/theme-name-list (mapcar #'(lambda (i) (nth 1 i)) lacquer/theme-list)
  "Theme name list.")


(defvar lacquer/setting nil
  "Setting instance.")

;;;;; Private

(defun lacquer-make-setting ()
  "Make setting instance."
  (setq lacquer/setting
        (make-instance 'lacquer-setting-cls :cls-cache-path lacquer/cache
                       :cls-theme-list lacquer/theme-list
                       :cls-font-list lacquer/font-list
                       :cls-cache-str (lacquer-read-path lacquer/cache)
                       :cls-setting (list (cons "theme" lacquer/default-theme)
                                          (cons "font" lacquer/default-font)
                                          (cons "font-size" lacquer/default-font-size)))
        )
  (cls-init lacquer/setting)
  )

;; Theme

(defmacro lacquer-theme-factory-macro (name load-name &rest config)
  "Theme factory macro.
NAME is theme package name.
LOAD-NAME is theme name.
CONFIG is theme config."
  `(progn
     (unless (package-installed-p (quote ,name))
       (package-install (quote ,name)))
     (when (symbolp (quote ,load-name))
       (disable-theme (cls-get lacquer/setting "theme"))
       (load-theme (quote ,load-name) t)
       ,@config

       (cls-set lacquer/setting "theme" (quote ,load-name))
       (message (format "<%s> loaded successfully."
                        (symbol-name (quote ,load-name)))))
     ))


(defun lacquer-theme-factory (theme)
  "Make params of theme factory function by THEME."
  (lacquer-interactive-factory (nth 1 theme) `(lacquer-theme-factory-macro ,@theme)))

;; Font

(defmacro lacquer-font-factory-macro (font)
  "Font factory macro by FONT."
  `(progn
     (set-face-attribute 'default nil :font (symbol-name (quote ,font)))
     (cls-set lacquer/setting "font" (quote ,font))
     ))


(defun lacquer-font-factory (font)
  "Make params of theme factory function by FONT."
  (lacquer-interactive-factory font `(lacquer-font-factory-macro ,font)))


(defun lacquer-interactive-factory (name body)
  "Generate interactive function factory by NAME and BODY."
  `(defun ,name ()
     "Lacquer interactive."
     (interactive)
     ,body
     ))


(defmacro lacquer-macro-factory (list func)
  "Theme function macro factory by LIST and FUNC."
  `(progn ,@(mapcar func (eval list))))

;; Font-size

(defun lacquer-font-size-operate (size)
  "Change font SIZE."
  (set-face-attribute 'default nil :height size)
  (cls-set lacquer/setting "font-size" size)
  (message "Current font size: <%s>." size))

;;;;; Public

;;;###autoload
(defun lacquer-current-theme ()
  "Current theme."
  (interactive)
  (message "Current theme: <%s>." (symbol-name (cls-get lacquer/setting "theme"))))


;;;###autoload
(defun lacquer-current-font ()
  "Current font."
  (interactive)
  (message "Current font: <%s>." (symbol-name (cls-get lacquer/setting "font"))))

;; Selector

(defun make-selector (list current select-list prompt &optional which)
  "Make selector by LIST of theme or font, CURRENT, SELECT-LIST and PROMPT or WHICH function."
  (let* ((selected (cl-loop with i = 0
                            for v in list
                            if (eq (if (functionp which) (funcall which v) v) current)
                            return i
                            else
                            do (cl-incf i)
                            finally return i))
         (func-str (ivy-read prompt
                             select-list
                             :sort nil
                             :require-match t
                             :preselect selected
                             ))
         (func (intern func-str)))
    (if (fboundp func)
        (funcall func)
      (message (format "<%s> is no existing." func-str)))
    )
  )


;;;###autoload
(defun lacquer-theme-selector ()
  "Theme selector."
  (interactive)
  (with-eval-after-load 'ivy
    (make-selector
     lacquer/theme-list
     (cls-get lacquer/setting "theme")
     lacquer/theme-name-list
     (format "Current theme is <%s>. Please choose: " (symbol-name (cls-get lacquer/setting "theme")))
     (lambda (v) (nth 1 v)))
    ))


;;;###autoload
(defun lacquer-font-selector ()
  "Theme selector."
  (interactive)
  (with-eval-after-load 'ivy
    (make-selector
     lacquer/font-list
     (cls-get lacquer/setting "font")
     lacquer/font-list
     (format "Current font is <%s>. Please choose: " (symbol-name (cls-get lacquer/setting "font")))
     )))


;;;###autoload
(defun lacquer-font-size-increase ()
  "Font size increase."
  (interactive)
  (let ((size (+ (cls-get lacquer/setting "font-size") lacquer/font-size-step)))
    (lacquer-font-size-operate size)))


;;;###autoload
(defun lacquer-font-size-decrease ()
  "Font size decrease."
  (interactive)
  (let ((size (- (cls-get lacquer/setting "font-size") lacquer/font-size-step)))
    (lacquer-font-size-operate size)))

;;;;; Keymaps

(defun lacquer-generate-map (map list prefix-key &optional which)
  "Generate key map by key MAP, LIST, PREFIX-KEY or WHICH func."
  (cl-loop with i = 0
           for v in list
           do (progn
                (define-key map (kbd (concat prefix-key " " (nth i lacquer/keys-map-index))) (if (functionp which) (funcall which v) v))
                (cl-incf i))
           finally return map
           ))


(defvar lacquer-mode-map (make-sparse-keymap "lacquer map")
  "Lacquer keymap.")


(define-key lacquer-mode-map (kbd lacquer/current-theme-key) 'lacquer-current-theme)
(define-key lacquer-mode-map (kbd lacquer/current-font-key) 'lacquer-current-font)
(define-key lacquer-mode-map (kbd lacquer/theme-selector-key) 'lacquer-theme-selector)
(define-key lacquer-mode-map (kbd lacquer/font-selector-key) 'lacquer-font-selector)
(define-key lacquer-mode-map (kbd lacquer/font-increase-key) 'lacquer-font-size-increase)
(define-key lacquer-mode-map (kbd lacquer/font-decrease-key) 'lacquer-font-size-decrease)

;; Minor-mode

(defun lacquer-start-up ()
  "Start up."
  (lacquer-make-setting)
  
  (lacquer-macro-factory lacquer/theme-list lacquer-theme-factory)
  (lacquer-macro-factory lacquer/font-list lacquer-font-factory)
  
  (lacquer-generate-map
   lacquer-mode-map
   lacquer/theme-list
   lacquer/theme-prefix-key
   (lambda (v) (nth 1 v)))
  (lacquer-generate-map
   lacquer-mode-map
   lacquer/font-list
   lacquer/font-prefix-key)
  
  (cls-call lacquer/setting)
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
