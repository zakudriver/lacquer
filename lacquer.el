;;; lacquer.el --- A util that switches theme/font and to configure cache  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 dingansich_kum0

;; Author: dingansich_kum0 <zy.hua1122@outlook.com>
;; URL: https://example.com/package-name.el
;; Version: 1.0
;; Package-Requires: ((emacs "25.2") (ivy "0.13.4"))
;; Keywords: tools

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

;;; Installation

;; put this file in your load-path.

;;; MELPA

;;; Install these required packages:

;; + cl-lib
;; + ivy

;;; Usage

;; Run one of these commands:

;; `lacquer-mode': start up lacquer-mode.

;;; Tips

;; (use-package lacquer
;;     :ensure nil
;;     :load-path "~/.emacs.d/site-lisp/lacquer"
;;     :hook
;;     (after-init . lacquer-mode)
;;     :custom
;;     (lacquer/cache "~/.emacs.d/lacquer")
;;     (lacquer/theme-list '((monokai-theme monokai)
;;                           (monokai-pro-theme monokai-pro)
;;                           (dracula-theme dracula)
;;                           (doom-themes doom-one-light)
;;                           (doom-themes doom-vibrant)
;;                           (doom-themes doom-nord)
;;                           (leuven-theme leuven (setq leuven-scale-outline-headlines nil))
;;                           (leuven-theme leuven-dark (setq leuven-scale-outline-headlines nil))))
;;     (lacquer/font-list '(Menlo
;;                          Roboto\ Mono
;;                          Anonymous\ Pro
;;                          FantasqueSansMono
;;                          FiraMono
;;                          Fira\ Code
;;                          Operator\ Mono
;;                          Inconsolata
;;                          Iosevka))
;;     (lacquer/default-font-size 130))


;;; Commentary:

;; Lacquer is a util that switches theme/font/font-size and to configure cache.
;; Use both the seletor and the shortcut key to switch themes/font.
;; Load previous theme/font/font-size after restarting the Emacs.
;; Each theme can be configured individually.
;; Download unused themes automatically with package.el.
;; Generate interactive function automatically.

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


(defcustom lacquer/auto-switch-mode 'orderly
  "Mode of switch theme automatically."
  :type '(choice
          (const :tag "Orderly" orderly)
          (const :tag "Random" random)))


(defcustom lacquer/auto-switch-time "12:00"
  "Time to switch themes every day."
  :type 'time)


(defcustom lacquer/default-theme 'monokai
  "Default theme."
  :type 'symbol)


(defcustom lacquer/font-list '(Menlo Fira\ Code)
  "Font list.
E.g: '((theme-package-name theme-name config)).
Required: theme-package-name theme-name.
Optional: config.Any function."
  :type 'list)


(defcustom lacquer/default-font 'Menlo
  "Default font."
  :type 'symbol)


(defcustom lacquer/default-font-size 135
  "Default font size."
  :type 'integer)


(defcustom lacquer/cache (concat user-emacs-directory ".lacquer")
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


(defcustom lacquer/theme-carousel-key "C-c T C"
  "Theme carousel bind key."
  :type 'string)


(defcustom lacquer/font-size-step 5
  "Change font size of step."
  :type 'integer)

;;;; Variables

(defvar lacquer/started nil
  "Lacquer mode has started.")


(defvar lacquer/theme-name-list (mapcar #'(lambda (i) (nth 1 i)) lacquer/theme-list)
  "Theme name list.")


(defvar lacquer/setting-instance nil
  "Setting instance.")

;;;;; Private

(defun lacquer-make-setting ()
  "Make setting instance."
  (setq lacquer/setting-instance
        (make-instance 'lacquer-setting-cls
                       :cls-cache-path lacquer/cache
                       :cls-theme-list lacquer/theme-list
                       :cls-font-list lacquer/font-list
                       :cls-cache-str (lacquer-read-path lacquer/cache)
                       :cls-setting (list (cons "theme" lacquer/default-theme)
                                          (cons "font" lacquer/default-font)
                                          (cons "font-size" lacquer/default-font-size))))
  (cls-init lacquer/setting-instance))

;; Theme

(defmacro lacquer-theme-factory-macro (index name load-name &rest config)
  "Theme factory macro.
INDEX: index in lacquer/theme-list
NAME: theme package name.
LOAD-NAME: theme name.
CONFIG: theme config."
  `(progn
     (unless (package-installed-p (quote ,name))
       (package-install (quote ,name)))
     (when (symbolp (quote ,load-name))
       (disable-theme (cls-get lacquer/setting-instance "theme"))
       (load-theme (quote ,load-name) t)
       ,@config

       (cls-set lacquer/setting-instance "theme" (quote ,load-name))
       (cls-set-index lacquer/setting-instance "theme" ,index)
       (message (format "<%s> loaded successfully."
                        (symbol-name (quote ,load-name)))))))


(defun lacquer-theme-factory (theme index)
  "Make params of theme factory function by THEME and INDEX."
  (lacquer-interactive-factory (nth 1 theme) `(lacquer-theme-factory-macro ,index ,@theme)))

;; Font

(defmacro lacquer-font-factory-macro (index font)
  "Font factory macro by FONT and INDEX."
  `(progn
     (set-face-attribute 'default nil :font (symbol-name (quote ,font)))
     (cls-set-index lacquer/setting-instance "font" ,index)
     (cls-set lacquer/setting-instance "font" (quote ,font))))


(defun lacquer-font-factory (font index)
  "Make params of theme factory function by FONT and INDEX."
  (lacquer-interactive-factory font `(lacquer-font-factory-macro ,index ,font)))


(defun lacquer-interactive-factory (name body)
  "Generate interactive function factory by NAME and BODY."
  `(defun ,name ()
     "Lacquer interactive."
     (interactive)
     ,body))


(defmacro lacquer-macro-factory (list func)
  "Theme function macro factory by LIST and callback FUNC."
  (let ((i -1))
    `(progn ,@(mapcar #'(lambda (v)
                          (cl-incf i)
                          (funcall func v i)) (eval list)))))

;; Font-size

(defun lacquer-font-size-operate (size)
  "Change font SIZE."
  (set-face-attribute 'default nil :height size)
  (cls-set lacquer/setting-instance "font-size" size)
  (message "Current font size: <%s>." size))

;; Automatically

(defun lacquer-auto-switch ()
  "Swtich theme automatically."
  
  )


;;;;; Public

;;;###autoload
(defun lacquer-current-theme ()
  "Current theme."
  (interactive)
  (message "Current theme: <%s>." (symbol-name (cls-get lacquer/setting-instance "theme"))))


;;;###autoload
(defun lacquer-current-font ()
  "Current font."
  (interactive)
  (message "Current font: <%s>." (symbol-name (cls-get lacquer/setting-instance "font"))))

;; Selector

(cl-defun lacquer-make-selector (&key list current select-list prompt which)
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
                             :preselect selected))
         (func (intern func-str)))
    (if (fboundp func)
        (funcall func)
      (message (format "<%s> is no existing." func-str)))))


;;;###autoload
(defun lacquer-theme-selector ()
  "Open theme selector in the minibuffer."
  (interactive)
  (with-eval-after-load 'ivy
    (lacquer-make-selector
     :list lacquer/theme-list
     :current (cls-get lacquer/setting-instance "theme")
     :select-list lacquer/theme-name-list
     :prompt (format "Current theme is <%s>. Please choose: " (symbol-name (cls-get lacquer/setting-instance "theme")))
     :which (lambda (v) (nth 1 v)))))


;;;###autoload
(defun lacquer-theme-carousel ()
  "Next theme from theme list."
  (interactive)
  (funcall (cls-get-next lacquer/setting-instance "theme" lacquer/auto-switch-mode)))


;;;###autoload
(defun lacquer-font-selector ()
  "Open font selector in the minibuffer."
  (interactive)
  (with-eval-after-load 'ivy
    (lacquer-make-selector
     :list lacquer/font-list
     :current (cls-get lacquer/setting-instance "font")
     :select-list lacquer/font-list
     :prompt (format "Current font is <%s>. Please choose: " (symbol-name (cls-get lacquer/setting-instance "font"))))))


;;;###autoload
(defun lacquer-font-size-increase ()
  "Font size increase."
  (interactive)
  (let ((size (+ (cls-get lacquer/setting-instance "font-size") lacquer/font-size-step)))
    (lacquer-font-size-operate size)))


;;;###autoload
(defun lacquer-font-size-decrease ()
  "Font size decrease."
  (interactive)
  (let ((size (- (cls-get lacquer/setting-instance "font-size") lacquer/font-size-step)))
    (lacquer-font-size-operate size)))

;;;;; Keymaps

(cl-defun lacquer-generate-map (&key map list prefix-key which)
  "Generate key map by key MAP, LIST, PREFIX-KEY or WHICH func."
  (cl-loop with i = 0
           for v in list
           do (progn
                (define-key map (kbd (concat prefix-key " " (nth i lacquer/keys-map-index))) (if (functionp which) (funcall which v) v))
                (cl-incf i))
           finally return map))


(defvar lacquer-mode-map (make-sparse-keymap "lacquer map")
  "Lacquer keymap.")


(define-key lacquer-mode-map (kbd lacquer/current-theme-key) 'lacquer-current-theme)
(define-key lacquer-mode-map (kbd lacquer/current-font-key) 'lacquer-current-font)
(define-key lacquer-mode-map (kbd lacquer/theme-selector-key) 'lacquer-theme-selector)
(define-key lacquer-mode-map (kbd lacquer/font-selector-key) 'lacquer-font-selector)
(define-key lacquer-mode-map (kbd lacquer/font-increase-key) 'lacquer-font-size-increase)
(define-key lacquer-mode-map (kbd lacquer/font-decrease-key) 'lacquer-font-size-decrease)
(define-key lacquer-mode-map (kbd lacquer/theme-carousel-key) 'lacquer-lacquer-theme-carousel)

;; Minor-mode

(defun lacquer-start-up (&optional auto)
  "Start up.
When AUTO is `no-nil' execute switch theme automatically."
  (lacquer-make-setting)
  
  (lacquer-macro-factory lacquer/theme-list lacquer-theme-factory)
  (lacquer-macro-factory lacquer/font-list lacquer-font-factory)
  
  (lacquer-generate-map
   :map lacquer-mode-map
   :list lacquer/theme-list
   :prefix-key lacquer/theme-prefix-key
   :which (lambda (v) (nth 1 v)))
  (lacquer-generate-map
   :map lacquer-mode-map
   :list lacquer/font-list
   :prefix-key lacquer/font-prefix-key)
  
  (cls-call lacquer/setting-instance)
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


(define-minor-mode lacquer-auto-mode
  "Minor mode to enable lacquer-auto."
  :init-value nil
  :group 'lacquer
  :keymap lacquer-mode-map
  :global t
  :lighter nil
  (unless lacquer/started
    (lacquer-start-up t)))


(provide 'lacquer)
;;; lacquer.el ends here
