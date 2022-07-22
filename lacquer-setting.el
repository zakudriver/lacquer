;;; lacquer-setting.el --- Setting about lacquer  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Setting class of lacquer.

;;; Install these required packages:

;; + cl-lib
;; + eieio

;;; Code:


(require 'cl-lib)
(require 'eieio)
(require 'lacquer-utils)


(defclass lacquer-setting-cls ()
  ((lacquer-cls-cache-path :initarg :cache-path
                           :initform (concat user-emacs-directory ".lacquer")
                           :type string
                           :custom string
                           :documentation "String of cache content.")
   (lacquer-cls-theme-list :initarg :theme-list
                           :initform nil
                           :type list
                           :custom list
                           :documentation "Theme list.")
   (lacquer-cls-font-list :initarg :font-list
                          :initform nil
                          :type list
                          :custom string
                          :documentation "Font list.")
   (lacquer-cls-setting :initarg :setting
                        :initform '(("theme" . nil)
                                    ("font" . nil)
                                    ("font-size" . 0)
                                    ("mode" . nil))
                        :custom (set (cons :tag "Theme" (const "theme") symbol)
                                     (cons :tag "Font" (const "font") symbol)
                                     (cons :tag "Font-size" (const "font-size") integer)
                                     (cons :tag "Mode" (const "mode") (choice
                                                                       (const :tag "Orderly" 'orderly)
                                                                       (const :tag "Random" 'random))))
                        :documentation "Currnet setting.(theme, font, font-size and mode).")
   (lacquer-cls-index :initarg :theme-index
                      :initform '(("theme" . 0)
                                  ("font" . 0))
                      :custom (set (cons :tag "Current index in the theme list." (const "theme") integer)
                                   (cons :tag "Current index in the font list." (const "font") integer))
                      :documentation "Current index in the theme and font list."))
  "Lacquer setting self.")


(cl-defmethod lacquer-cls-check-setting ((this lacquer-setting-cls) key value)
  "Check THIS's setting value by KEY and VALUE, and return right value."
  (lacquer-cls-get this key)
  (cond ((string= key "theme")
         (let ((index (lacquer-list-included-p
                       (oref this lacquer-cls-theme-list) value (lambda (v) (nth 1 v)))))
           (if index
               value (lacquer-cls-get this key))))
        ((string= key "font")
         (if (and (lacquer-list-included-p (oref this lacquer-cls-font-list) value) (lacquer-font-installed-p value))
             value (lacquer-cls-get this key)))
        ((string= key "font-size")
         (if (integerp value) value (lacquer-cls-get this key)))
        ((string= key "mode")
         (if (symbolp value) value (lacquer-cls-get this key)))
        (t nil)))


(cl-defmethod lacquer-cls-init-setting ((this lacquer-setting-cls))
  "Init THIS."
  (let* ((cache-path (oref this lacquer-cls-cache-path))
         (setting
          (if (file-exists-p cache-path)
              (with-temp-buffer
                (insert-file-contents cache-path)
                (read (current-buffer)))
            (oref this lacquer-cls-setting))))
    (cl-loop for (k . v) in setting
             do (setf (cdr
                       (assoc k (oref this lacquer-cls-setting)))
                      (lacquer-cls-check-setting this k v)))))


(cl-defmethod lacquer-cls-call ((this lacquer-setting-cls))
  "Initialization call THIS's theme, font and font-sie."
  (cl-loop for (k . v) in (oref this lacquer-cls-setting)
           do (cond ((string= k "font-size")
                     (set-face-attribute 'default nil :height v))
                    ((string= k "mode")
                     nil)
                    (t
                     (funcall v)))))


(cl-defmethod lacquer-cls-get ((this lacquer-setting-cls) key)
  "Get current setting(theme, font and font-size) from THIS by KEY.
Return symbol of theme or font, int of font-size."
  (cdr (assoc key (oref this lacquer-cls-setting))))


(cl-defmethod lacquer-cls-set ((this lacquer-setting-cls) key value)
  "Set KEY and VALUE to THIS's setting(theme, font and font-size)."
  (unless (eq value (cdr
                     (assoc key (oref this lacquer-cls-setting))))
    (setf (cdr
           (assoc key (oref this lacquer-cls-setting))) value)
    (lacquer-cls-write-cache this)))


(cl-defmethod lacquer-cls-write-cache ((this lacquer-setting-cls))
  "Wirte THIS's' setting to cache."
  (with-temp-file (oref this lacquer-cls-cache-path)
    (prin1 (oref this lacquer-cls-setting) (current-buffer))))


(cl-defmethod lacquer-cls-get-index ((this lacquer-setting-cls) key)
  "Get THIS's index from KEY."
  (cdr (assoc key (oref this lacquer-cls-index))))


(cl-defmethod lacquer-cls-get-list ((this lacquer-setting-cls) key)
  "Get THIS's theme or font list from KEY."
  (cond ((string= key "theme")
         (oref this lacquer-cls-theme-list))
        ((string= key "font")
         (oref this lacquer-cls-font-list))
        (t
         nil)))


(cl-defmethod lacquer-cls-next-index ((this lacquer-setting-cls) key)
  "Get THIS's next index from KEY."
  (let ((next (+ (lacquer-cls-get-index this key) 1)))
    (if (= next (length (lacquer-cls-get-list this key)))
        0
      next)))


(cl-defmethod lacquer-cls-set-index ((this lacquer-setting-cls) key value)
  "Set THIS's index by KEY and VALUE."
  (setf (cdr
         (assoc key (oref this lacquer-cls-index))) value))


(cl-defmethod lacquer-cls-get-next ((this lacquer-setting-cls) key)
  "Get THIS's next KEY of theme or font.
MODE is lacquer-auto-switch-mode."
  (let ((i 0)
        (mode (lacquer-cls-get this "mode"))
        (list (lacquer-cls-get-list this key)))
    (cond ((eq mode 'orderly)
           (setq i (lacquer-cls-next-index this key)))
          ((eq mode 'random)
           (setq i (random (length list))))
          (t
           (setq i 0)))
    (nth 1 (nth i list))))


(provide 'lacquer-setting)

;;; lacquer-setting.el ends here
