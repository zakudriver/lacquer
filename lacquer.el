;;; lacquer.el --- Switch theme/font by selecting from a cache  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 zakudriver

;; Author: zakudriver <zy.hua1122@outlook.com>
;; URL: https://github.com/zakudriver/lacquer
;; Version: 1.0
;; Package-Requires: ((emacs "25.2"))
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
;; + seq

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
;;     (lacquer-cache "~/.emacs.d/.lacquer.el")
;;     (lacquer-theme-list '((monokai-theme monokai)
;;                           (monokai-pro-theme monokai-pro)
;;                           (dracula-theme dracula)
;;                           (doom-themes doom-one-light)
;;                           (doom-themes doom-vibrant)
;;                           (doom-themes doom-nord)
;;                           (leuven-theme leuven (setq leuven-scale-outline-headlines nil))
;;                           (leuven-theme leuven-dark (setq leuven-scale-outline-headlines nil))))
;;     (lacquer-font-list '(Menlo
;;                          Roboto\ Mono
;;                          Anonymous\ Pro
;;                          FantasqueSansMono
;;                          FiraMono
;;                          Fira\ Code
;;                          Operator\ Mono
;;                          Inconsolata
;;                          Iosevka))
;;     (lacquer-default-font-size 130))


;;; Commentary:

;; - Use both the seletor and the shortcut key to switch themes/font.
;; - Load previous theme/font/font-size after restarting the Emacs.
;; - Each theme can be configured individually.
;; - Download unused themes automatically with package.el.
;; - Generate interactive function automatically.
;; - Automatically change themes depending on a time schedule created by users.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'seq)
(require 'lacquer-utils)
(require 'lacquer-setting)
(require 'lacquer-automation)


;;;; Constants

(defconst lacquer-mode-list '(orderly random)
  "Mode list.")

;;;; Customization

(defgroup lacquer nil
  "Settings for `lacquer'."
  :prefix "lacquer-"
  :group 'utils)


(defcustom lacquer-theme-list '((monokai-theme monokai))
  "Theme list.
E.g: '((theme-package-name theme-name config)).
Required: theme-package-name theme-name.
Optional: config.Any function."
  :group 'lacquer
  :type '(alist :value-type (group symbol symbol function)))


(defcustom lacquer-auto-switch-mode (car lacquer-mode-list)
  "Mode of switch theme automatically."
  :group 'lacquer
  :type '(choice
          (const :tag "Orderly" orderly)
          (const :tag "Random" random)))


(defcustom lacquer-auto-switch-time (lacquer-time-word-seconds 1 "hour")
  "When it's list,  switch themes at time of list item every day.
When it's integer, switch themes for every some seconds"
  :group 'lacquer
  :type '(choice (integer :tag "Time of relativetime." :value 3600)
                 (list :tag "List of switching." :value '("10:00" "15:00" "18:00"))))


(defcustom lacquer-default-theme 'monokai
  "Default theme."
  :group 'lacquer
  :type 'symbol)


(defcustom lacquer-font-list '(Menlo Fira\ Code)
  "Font list."
  :type '(group symbol))


(defcustom lacquer-default-font 'Menlo
  "Default font."
  :group 'lacquer
  :type 'symbol)


(defcustom lacquer-default-font-size 135
  "Default font size."
  :type 'integer)


(defcustom lacquer-cache (concat user-emacs-directory ".lacquer")
  "Path of cache."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-theme-prefix-key "C-c T"
  "Trigger theme of prefix key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-font-prefix-key "C-c F"
  "Trigger of prefix key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-keys-map-index
  (append (lacquer-generate-keys-index-list)
          '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
          (lacquer-generate-keys-index-list "Y")
          (lacquer-generate-keys-index-list "Z"))
  "Keys map index."
  :group 'lacquer
  :type '(group string))


(defcustom lacquer-theme-selector-key "C-c T S"
  "Theme selector bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-font-selector-key "C-c F S"
  "Font selector bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-mode-selector-key "C-c T M"
  "Mode selector bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-font-increase-key "C-c F +"
  "Font increase bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-font-decrease-key "C-c F _"
  "Font increase bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-current-theme-key "C-c T 0"
  "Current theme bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-current-font-key "C-c F 0"
  "Current theme bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-theme-carousel-key "C-c T C"
  "Theme carousel bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-start-auto-switch-key "C-c T A"
  "Start swtich theme automatically bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-stop-auto-switch-key "C-c T P"
  "Stop swtich theme automatically bind key."
  :group 'lacquer
  :type 'string)


(defcustom lacquer-font-size-step 5
  "Change font size of step."
  :group 'lacquer
  :type 'integer)

;;;; Variables

(defvar lacquer-started nil
  "Lacquer mode has started.")


(defvar lacquer-theme-name-list (mapcar (lambda (i) (nth 1 i)) lacquer-theme-list)
  "Theme name list.")


(defvar lacquer-setting-instance nil
  "Setting instance.")


(defvar lacquer-automation-instance nil
  "Automation instance.")

;;;;; Private

(defun lacquer-new-setting ()
  "New setting instance."
  (unless lacquer-setting-instance
    (setq lacquer-setting-instance
          (make-instance 'lacquer-setting-cls
                         :cache-path lacquer-cache
                         :theme-list lacquer-theme-list
                         :font-list lacquer-font-list
                         :setting (list (cons "theme" lacquer-default-theme)
                                        (cons "font" lacquer-default-font)
                                        (cons "font-size" lacquer-default-font-size)
                                        (cons "mode" lacquer-auto-switch-mode))))
    (lacquer-cls-init-setting lacquer-setting-instance)))


(defun lacquer-new-automation ()
  "New automation instance."
  (unless lacquer-automation-instance
    (setq lacquer-automation-instance
          (make-instance 'lacquer-automation-cls
                         :time lacquer-auto-switch-time))))


(defun lacquer-font-installed-filter ()
  "Filter font list if it's installed."
  (setq lacquer-font-list
        (seq-filter #'lacquer-font-installed-p lacquer-font-list)))


(defun lacquer-interactive-factory (name body)
  "Generate interactive function factory by NAME and BODY."
  (defalias name
    `(lambda ()
       "Lacquer interactive."
       (interactive)
       ,body)))

;; Theme

(defmacro lacquer-theme-factory-macro (index name load-name &rest config)
  "Theme factory macro.
INDEX: index in lacquer-theme-list
NAME: theme package name.
LOAD-NAME: theme name.
CONFIG: theme config."
  `(progn
     (unless (package-installed-p (quote ,name))
       (package-install (quote ,name)))
     (when (symbolp (quote ,load-name))
       (disable-theme (lacquer-cls-get lacquer-setting-instance "theme"))
       (load-theme (quote ,load-name) t)
       ,@config

       (lacquer-cls-set lacquer-setting-instance "theme" (quote ,load-name))
       (lacquer-cls-set-index lacquer-setting-instance "theme" ,index)
       (message "<%s> loaded successfully."
                (symbol-name (quote ,load-name))))))


(defun lacquer-theme-factory (theme index)
  "Make params of theme factory function by THEME and INDEX."
  (lacquer-interactive-factory (nth 1 theme) `(lacquer-theme-factory-macro ,index ,@theme)))

;; Font

(defmacro lacquer-font-factory-macro (index font)
  "Font factory macro by FONT and INDEX."
  `(progn
     (set-face-attribute 'default nil :font (symbol-name (quote ,font)))
     (lacquer-cls-set-index lacquer-setting-instance "font" ,index)
     (lacquer-cls-set lacquer-setting-instance "font" (quote ,font))))


(defun lacquer-font-factory (font index)
  "Make params of theme factory function by FONT and INDEX."
  (lacquer-interactive-factory font `(lacquer-font-factory-macro ,index ,font)))


;; Font-size

(defun lacquer-font-size-operate (size)
  "Change font SIZE."
  (set-face-attribute 'default nil :height size)
  (lacquer-cls-set lacquer-setting-instance "font-size" size)
  (message "Current font size: <%s>." size))

;; Automatically

(defun lacquer-switch-next-theme ()
  "Switch next theme from lacquer-auto-switch-mode."
  (funcall (lacquer-cls-get-next lacquer-setting-instance "theme")))

;;;;; Public

;;;###autoload
(defun lacquer-current-theme ()
  "Current theme."
  (interactive)
  (message "Current theme: <%s>." (symbol-name (lacquer-cls-get lacquer-setting-instance "theme"))))


;;;###autoload
(defun lacquer-current-font ()
  "Current font."
  (interactive)
  (message "Current font: <%s>." (symbol-name (lacquer-cls-get lacquer-setting-instance "font"))))

;; Selector

(cl-defun lacquer-make-selector (&key list current select-list prompt which func)
  "Make selector by LIST of theme or font, CURRENT,
SELECT-LIST and PROMPT or WHICH function, FUNC is callback of select."
  (let* ((selected (cl-loop with i = 0
                            for v in list
                            if (eq (if (functionp which) (funcall which v) v) current)
                            return (symbol-name (if (functionp which) (funcall which v) v))
                            else
                            do (cl-incf i)
                            finally return i))
         (str (completing-read prompt
                               select-list
                               nil
                               t
                               nil
                               nil
                               selected)))
    (if (functionp func)
        (funcall func str))))


;;;###autoload
(defun lacquer-theme-selector ()
  "Open theme selector in the minibuffer."
  (interactive)
  (lacquer-make-selector
   :list lacquer-theme-list
   :current (lacquer-cls-get lacquer-setting-instance "theme")
   :select-list lacquer-theme-name-list
   :prompt (format "Current theme is <%s>. Please choose: " (symbol-name (lacquer-cls-get lacquer-setting-instance "theme")))
   :which (lambda (v) (nth 1 v))
   :func (lambda (value)
           (let ((theme (intern value)))
             (if (fboundp theme)
                 (funcall theme)
               (message "<%s> is no existing." theme))))))


;;;###autoload
(defun lacquer-font-selector ()
  "Open font selector in the minibuffer."
  (interactive)
  (lacquer-make-selector
   :list lacquer-font-list
   :current (lacquer-cls-get lacquer-setting-instance "font")
   :select-list lacquer-font-list
   :prompt (format "Current font is <%s>. Please choose: " (symbol-name (lacquer-cls-get lacquer-setting-instance "font")))
   :func (lambda (value)
           (let ((font (intern value)))
             (if (fboundp font)
                 (funcall font)
               (message "<%s> is no existing." font))))))


;;;###autoload
(defun lacquer-mode-selector ()
  "Open mode selector in the minibuffer."
  (interactive)
  (lacquer-make-selector
   :list lacquer-mode-list
   :current (lacquer-cls-get lacquer-setting-instance "mode")
   :select-list lacquer-mode-list
   :prompt (format "Current mode is <%s>. Please choose: " (symbol-name (lacquer-cls-get lacquer-setting-instance "mode")))
   :func (lambda (value)
           (lacquer-cls-set lacquer-setting-instance "mode" (intern value)))))

;; Carousel

;;;###autoload
(defun lacquer-theme-carousel ()
  "Next theme from theme list."
  (interactive)
  (lacquer-switch-next-theme))

;; Font size

;;;###autoload
(defun lacquer-font-size-increase ()
  "Font size increase."
  (interactive)
  (let ((size (+ (lacquer-cls-get lacquer-setting-instance "font-size") lacquer-font-size-step)))
    (lacquer-font-size-operate size)))


;;;###autoload
(defun lacquer-font-size-decrease ()
  "Font size decrease."
  (interactive)
  (let ((size (- (lacquer-cls-get lacquer-setting-instance "font-size") lacquer-font-size-step)))
    (lacquer-font-size-operate size)))


;;;###autoload
(defun lacquer-start-auto-switch ()
  "Start swtich theme automatically."
  (interactive)
  (lacquer-new-automation)
  (lacquer-cls-run lacquer-automation-instance #'lacquer-switch-next-theme))


;;;###autoload
(defun lacquer-stop-auto-switch ()
  "Stop switch theme automatically."
  (interactive)
  (lacquer-cls-stop lacquer-automation-instance))

;;;;; Keymaps

(cl-defun lacquer-generate-map (&key map list prefix-key which)
  "Generate key map by key MAP, LIST, PREFIX-KEY or WHICH func."
  (cl-loop with i = 0
           for v in list
           do (progn
                (define-key map (kbd (concat prefix-key " " (nth i lacquer-keys-map-index))) (if (functionp which) (funcall which v) v))
                (cl-incf i))
           finally return map))


(defvar lacquer-mode-map (let ((km (make-sparse-keymap "lacquer map")))
                           (define-key km (kbd lacquer-current-theme-key) 'lacquer-current-theme)
                           (define-key km (kbd lacquer-current-font-key) 'lacquer-current-font)
                           (define-key km (kbd lacquer-theme-selector-key) 'lacquer-theme-selector)
                           (define-key km (kbd lacquer-font-selector-key) 'lacquer-font-selector)
                           (define-key km (kbd lacquer-font-increase-key) 'lacquer-font-size-increase)
                           (define-key km (kbd lacquer-font-decrease-key) 'lacquer-font-size-decrease)
                           (define-key km (kbd lacquer-theme-carousel-key) 'lacquer-theme-carousel)
                           (define-key km (kbd lacquer-mode-selector-key) 'lacquer-mode-selector)
                           (define-key km (kbd lacquer-start-auto-switch-key) 'lacquer-start-auto-switch)
                           (define-key km (kbd lacquer-stop-auto-switch-key) 'lacquer-stop-auto-switch)
                           km)
  "Lacquer keymap.")

;; Minor-mode

(defun lacquer-start-up (&optional auto)
  "Start up.
When AUTO is `no-nil' execute switch theme automatically."
  (lacquer-font-installed-filter)
  (lacquer-new-setting)

  (lacquer-mapc-incf #'lacquer-theme-factory lacquer-theme-list)
  (lacquer-mapc-incf #'lacquer-font-factory lacquer-font-list)
  
  (lacquer-generate-map
   :map lacquer-mode-map
   :list lacquer-theme-list
   :prefix-key lacquer-theme-prefix-key
   :which (lambda (v) (nth 1 v)))
  (lacquer-generate-map
   :map lacquer-mode-map
   :list lacquer-font-list
   :prefix-key lacquer-font-prefix-key)
  
  (lacquer-cls-call lacquer-setting-instance)
  (if auto
      (lacquer-start-auto-switch))
  (setq lacquer-started t))


;;;###autoload
(define-minor-mode lacquer-mode
  "Minor mode to enable lacquer."
  :init-value nil
  :group 'lacquer
  :keymap lacquer-mode-map
  :global t
  :lighter nil
  (unless lacquer-started
    (lacquer-start-up)))


;;;###autoload
(define-minor-mode lacquer-auto-mode
  "Minor mode to enable lacquer-auto."
  :init-value nil
  :group 'lacquer
  :keymap lacquer-mode-map
  :global t
  :lighter nil
  (unless lacquer-started
    (lacquer-start-up t)))


(provide 'lacquer)

;;; lacquer.el ends here
