;;; setting.el --- Setting about lacquer  -*- lexical-binding: t; -*-

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
  ((cls-cache-path :initarg :cls-cache-path
                   :initform (concat user-emacs-directory ".lacquer")
                   :type string
                   :custom string
                   :documentation "String of cache content.")
   (cls-theme-list :initarg :cls-theme-list
                   :initform nil
                   :type list
                   :custom list
                   :documentation "Theme list.")
   (cls-font-list :initarg :cls-font-list
                  :initform nil
                  :type list
                  :custom list
                  :documentation "Font list.")
   (cls-cache-str :initarg :cls-cache-str
                  :initform ""
                  :type string
                  :custom string
                  :documentation "Cache(string) of read `lacquer-cache'.")
   (cls-setting :initarg :cls-setting
                :initform '(("theme" . nil)
                            ("font" . nil)
                            ("font-size" . 0)
                            ("mode" . nil))
                :type list
                :custom list
                :documentation "Currnet setting.(theme, font, font-size and mode).")
   (cls-index :initarg :cls-theme-index
              :initform '(("theme" . 0)
                          ("font" . 0))
              :type list
              :custom list
              :documentation "Current theme/font index."))
  "Lacquer setting self.")


(cl-defmethod cls-parse-cache ((this lacquer-setting-cls) key)
  "Parse THIS's string of cache by KEY.
Return string."
  (replace-regexp-in-string (regexp-quote (format "%s=" key))
                            ""
                            (if (string-match (format "^%s=.+$" key) (oref this cls-cache-str))
                                (match-string 0 (oref this cls-cache-str))
                              "")))


(cl-defmethod cls-check-setting ((this lacquer-setting-cls) key value)
  "Check THIS's setting value by KEY and VALUE, and return right value."
  (if (string= value "")
      (cls-get this key)
    (cond ((string= key "theme")
           (let ((theme (intern value)))
             (let ((index (lacquer-list-include
                           (oref this cls-theme-list) theme (lambda (v) (nth 1 v)))))
               (if index
                   theme (cls-get this key)))))
          ((string= key "font")
           (let ((font (intern value)))
             (if (and (lacquer-list-include (oref this cls-font-list) font) (lacquer-font-installed-p value))
                 font (cls-get this key))))
          ((string= key "font-size")
           (if value (string-to-number value) (cls-get this key)))
          ((string= key "mode")
           (if value (intern value) (cls-get this key)))
          (t nil))))


(cl-defmethod cls-init ((this lacquer-setting-cls))
  "Init THIS."
  (cl-loop for (k . v) in (oref this cls-setting)
           do (setf (cdr
                     (assoc k (oref this cls-setting)))
                    (cls-check-setting this k (cls-parse-cache this k)))))


(cl-defmethod cls-call ((this lacquer-setting-cls))
  "Initialization call THIS's theme, font and font-sie."
  (cl-loop for (k . v) in (oref this cls-setting)
           do (cond ((string= k "font-size")
                     (set-face-attribute 'default nil :height v))
                    ((string= k "mode")
                     nil)
                    (t
                     (funcall v)))))


(cl-defmethod cls-get ((this lacquer-setting-cls) key)
  "Get current setting(theme, font and font-size) from THIS by KEY.
Return symbol of theme or font, int of font-size."
  (cdr (assoc key (oref this cls-setting))))


(cl-defmethod cls-set ((this lacquer-setting-cls) key value)
  "Set KEY and VALUE to THIS's setting(theme, font and font-size)."
  (unless (eq value (cdr
                     (assoc key (oref this cls-setting))))
    (setf (cdr
           (assoc key (oref this cls-setting))) value)
    (cls-write-cache this)))


(cl-defmethod cls-write-cache ((this lacquer-setting-cls))
  "Wirte THIS's' setting to cache."
  (write-region
   (cl-loop with str = ""
            for (k . v) in (oref this cls-setting)
            do (setq str (concat str (format "%s=" k) (format "%s" v) "\n"))
            finally return str)
   nil (oref this cls-cache-path)))


(cl-defmethod cls-get-index ((this lacquer-setting-cls) key)
  "Get THIS's index from KEY."
  (cdr (assoc key (oref this cls-index))))


(cl-defmethod cls-get-list ((this lacquer-setting-cls) key)
  "Get THIS's theme or font list from KEY."
  (cond ((string= key "theme")
         (oref this cls-theme-list))
        ((string= key "font")
         (oref this cls-font-list))
        (t
         nil)))


(cl-defmethod cls-next-index ((this lacquer-setting-cls) key)
  "Get THIS's next index from KEY."
  (let ((next (+ (cls-get-index this key) 1)))
    (if (= next (length (cls-get-list this key)))
        0
      next)))


(cl-defmethod cls-set-index ((this lacquer-setting-cls) key value)
  "Set THIS's index by KEY and VALUE."
  (setf (cdr
         (assoc key (oref this cls-index))) value))


(cl-defmethod cls-get-next ((this lacquer-setting-cls) key mode)
  "Get THIS's next KEY of theme or font.
MODE is lacquer/auto-switch-mode."
  (let ((i 0)
        (list (cls-get-list this key)))
    (cond ((eq mode 'orderly)
           (setq i (cls-next-index this key)))
          ((eq mode 'random)
           (setq i (random (length list))))
          (t
           (setq i 0)))
    (nth 1 (nth i list))))


(provide 'setting)

;;; setting.el ends here
(setq a "theme=doom-tomorrow-night
font=Iosevka
font-size=135")

(replace-regexp-in-string (regexp-quote (format "%s=" "mode"))
                          ""
                          (if (string-match (format "^%s=.+$" "mode") a)
                              (match-string 0 a) ""))
