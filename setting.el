;;; setting.el --- Setting about lacquer  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Setting class of lacquer.

;;; Install these required packages:

;; + cl-lib
;; + eieio

;;; Code:


(require 'cl-lib)
(require 'eieio)
(require 'utils)


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
                          :custom list
                          :documentation "Font list.")
   (lacquer-cls-cache-str :initarg :cache-str
                          :initform ""
                          :type string
                          :custom string
                          :documentation "Cache(string) of read `lacquer-cache'.")
   (lacquer-cls-setting :initarg :setting
                        :initform '(("theme" . nil)
                                    ("font" . nil)
                                    ("font-size" . 0)
                                    ("mode" . nil))
                        :type list
                        :custom list
                        :documentation "Currnet setting.(theme, font, font-size and mode).")
   (lacquer-cls-index :initarg :theme-index
                      :initform '(("theme" . 0)
                                  ("font" . 0))
                      :type list
                      :custom list
                      :documentation "Current theme/font index."))
  "Lacquer setting self.")


(cl-defmethod lacquer-cls-parse-cache ((this lacquer-setting-cls) key)
  "Parse THIS's string of cache by KEY.
Return string."
  (replace-regexp-in-string (regexp-quote (format "%s=" key))
                            ""
                            (if (string-match (format "^%s=.+$" key) (oref this lacquer-cls-cache-str))
                                (match-string 0 (oref this lacquer-cls-cache-str))
                              "")))


(cl-defmethod lacquer-cls-check-setting ((this lacquer-setting-cls) key value)
  "Check THIS's setting value by KEY and VALUE, and return right value."
  (if (string= value "")
      (lacquer-cls-get this key)
    (cond ((string= key "theme")
           (let ((theme (intern value)))
             (let ((index (lacquer-list-include
                           (oref this lacquer-cls-theme-list) theme (lambda (v) (nth 1 v)))))
               (if index
                   theme (lacquer-cls-get this key)))))
          ((string= key "font")
           (let ((font (intern value)))
             (if (and (lacquer-list-include (oref this lacquer-cls-font-list) font) (lacquer-font-installed-p value))
                 font (lacquer-cls-get this key))))
          ((string= key "font-size")
           (if value (string-to-number value) (lacquer-cls-get this key)))
          ((string= key "mode")
           (if value (intern value) (lacquer-cls-get this key)))
          (t nil))))


(cl-defmethod lacquer-cls-init-setting ((this lacquer-setting-cls))
  "Init THIS."
  (cl-loop for (k . v) in (oref this lacquer-cls-setting)
           do (setf (cdr
                     (assoc k (oref this lacquer-cls-setting)))
                    (lacquer-cls-check-setting this k (lacquer-cls-parse-cache this k)))))


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
  (write-region
   (cl-loop with str = ""
            for (k . v) in (oref this lacquer-cls-setting)
            do (setq str (concat str (format "%s=" k) (format "%s" v) "\n"))
            finally return str)
   nil (oref this lacquer-cls-cache-path)))


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


(provide 'setting)

;;; setting.el ends here
