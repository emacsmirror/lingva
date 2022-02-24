;;; lingva.el --- access google translate via lingva.ml  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 marty hiatt

;; Author: marty hiatt <martianhiatus@riseup.net>
;; Keywords: convenience, translation

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interact with the Lingva.ml API from within Emacs.

;;; Code:

(require 'json)

(defvar url-http-end-of-headers)
(defvar url-request-method)

(defgroup lingva nil
  "Interact with Linva.ml instances."
  :prefix "linvga"
  :group 'external)

(defcustom lingva-instance "https://lingva.ml"
  "The lingva instance to use."
  :type 'string)

;; this can be updated by running `lingva-return-langs-as-list'
(defvar lingva-languages
  '(("auto" . "Detect")
    ("af" . "Afrikaans")
    ("sq" . "Albanian")
    ("am" . "Amharic")
    ("ar" . "Arabic")
    ("hy" . "Armenian")
    ("az" . "Azerbaijani")
    ("eu" . "Basque")
    ("be" . "Belarusian")
    ("bn" . "Bengali")
    ("bs" . "Bosnian")
    ("bg" . "Bulgarian")
    ("ca" . "Catalan")
    ("ceb" . "Cebuano")
    ("ny" . "Chichewa")
    ("zh" . "Chinese")
    ("zh_HANT" . "Chinese (Traditional)")
    ("co" . "Corsican")
    ("hr" . "Croatian")
    ("cs" . "Czech")
    ("da" . "Danish")
    ("nl" . "Dutch")
    ("en" . "English")
    ("eo" . "Esperanto")
    ("et" . "Estonian")
    ("tl" . "Filipino")
    ("fi" . "Finnish")
    ("fr" . "French")
    ("fy" . "Frisian")
    ("gl" . "Galician")
    ("ka" . "Georgian")
    ("de" . "German")
    ("el" . "Greek")
    ("gu" . "Gujarati")
    ("ht" . "Haitian Creole")
    ("ha" . "Hausa")
    ("haw" . "Hawaiian")
    ("iw" . "Hebrew")
    ("hi" . "Hindi")
    ("hmn" . "Hmong")
    ("hu" . "Hungarian")
    ("is" . "Icelandic")
    ("ig" . "Igbo")
    ("id" . "Indonesian")
    ("ga" . "Irish")
    ("it" . "Italian")
    ("ja" . "Japanese")
    ("jw" . "Javanese")
    ("kn" . "Kannada")
    ("kk" . "Kazakh")
    ("km" . "Khmer")
    ("rw" . "Kinyarwanda")
    ("ko" . "Korean")
    ("ku" . "Kurdish (Kurmanji)")
    ("ky" . "Kyrgyz")
    ("lo" . "Lao")
    ("la" . "Latin")
    ("lv" . "Latvian")
    ("lt" . "Lithuanian")
    ("lb" . "Luxembourgish")
    ("mk" . "Macedonian")
    ("mg" . "Malagasy")
    ("ms" . "Malay")
    ("ml" . "Malayalam")
    ("mt" . "Maltese")
    ("mi" . "Maori")
    ("mr" . "Marathi")
    ("mn" . "Mongolian")
    ("my" . "Myanmar (Burmese)")
    ("ne" . "Nepali")
    ("no" . "Norwegian")
    ("or" . "Odia (Oriya)")
    ("ps" . "Pashto")
    ("fa" . "Persian")
    ("pl" . "Polish")
    ("pt" . "Portuguese")
    ("pa" . "Punjabi")
    ("ro" . "Romanian")
    ("ru" . "Russian")
    ("sm" . "Samoan")
    ("gd" . "Scots Gaelic")
    ("sr" . "Serbian")
    ("st" . "Sesotho")
    ("sn" . "Shona")
    ("sd" . "Sindhi")
    ("si" . "Sinhala")
    ("sk" . "Slovak")
    ("sl" . "Slovenian")
    ("so" . "Somali")
    ("es" . "Spanish")
    ("su" . "Sundanese")
    ("sw" . "Swahili")
    ("sv" . "Swedish")
    ("tg" . "Tajik")
    ("ta" . "Tamil")
    ("tt" . "Tatar")
    ("te" . "Telugu")
    ("th" . "Thai")
    ("tr" . "Turkish")
    ("tk" . "Turkmen")
    ("uk" . "Ukrainian")
    ("ur" . "Urdu")
    ("ug" . "Uyghur")
    ("uz" . "Uzbek")
    ("vi" . "Vietnamese")
    ("cy" . "Welsh")
    ("xh" . "Xhosa")
    ("yi" . "Yiddish")
    ("yo" . "Yoruba")
    ("zu" . "Zulu"))
  "The list of languages to choose from.
Can be used for either source or target for a lingva query.")

;; let user choose these from lingva-languages
(defcustom lingva-source "auto"
  "The default language to translate from, as a two letter code.
For details of what languages are availble and their
corresponding codes, see `lingva-languages'."
  :type 'string)

(defcustom lingva-target "en"
  "The default language to translate to, as a two letter code.
For details of what languages are availble and their
corresponding codes, see `lingva-languages'."
  :type 'string)

(defvar lingva-search-url
  (concat lingva-instance "/api/v1/" lingva-source "/" lingva-target "/")
  "The URL for a lingva query.")

(defvar lingva-languages-url
  (concat lingva-instance "/api/v1/languages")
  "The URL for a lingva source and target languages list query.")

;; if don't want hard-coded linva-languages list, ask the server
;; then make a list:
(defun lingva-get-languages ()
  "Return the languages supported by the server."
  (let* ((url-request-method "GET")
         (response-buffer (url-retrieve-synchronously
                           lingva-languages-url)))
    (setq lingva-languages
          (with-current-buffer response-buffer
            (goto-char (point-min))
            (search-forward "\n\n")
            (json-read)))))

(defun lingva-return-langs-as-list ()
  "Return a list of cons cells containing languages supported by the server."
  (let* ((lingva-langs-response (lingva-get-languages))
         (langs-response-vector (cdar lingva-langs-response))
         (langs-response-list (append langs-response-vector nil)))
    (mapcar (lambda (x)
              (cons (cdar x)
                    (cdadr x)))
            langs-response-list)))

;;;###autoload
(defun lingva-translate (&optional arg)
  "Prompt for TEXT to translate and return the translation in a buffer.
By default, in order, text is the current region, or current
word, or user input. With a single prefix ARG, prompt to specify
a source language different to `lingva-source'. With a second
prefix ARG, promp to to specify both a source language different
to `lingva-source' and a target language different to
`lingva-target'."
  (interactive "P")
  (let* ((url-request-method "GET")
         (lingva-languages (mapcar (lambda (x)
                                     (cons (cdr x) (car x)))
                                   lingva-languages))
         (lingva-source (if (and arg (>= (car arg) 4)) ; if 1 or 2 prefix args
                            (let ((response
                                   (completing-read "Source language: "
                                                    lingva-languages)))
                              (cdr (assoc response lingva-languages)))
                          lingva-source))
         (lingva-target (if (equal arg '(16)) ; only if 2 prefix args
                            (let ((response
                                   (completing-read "Target language: "
                                                    lingva-languages)))
                              (cdr (assoc response lingva-languages)))
                          lingva-target))
         (region (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))))
         (text
          (read-string (format "Translate (%s): " (or region (current-word) ""))
                       nil nil (or region (current-word))))
         (query (url-hexify-string text))
         (response-buffer (url-retrieve-synchronously
                           (concat lingva-instance "/api/v1/"
                                   lingva-source "/"
                                   lingva-target "/"
                                   query))))
    (with-current-buffer response-buffer
      (if (not (string-prefix-p "2" (lingva--status)))
          (switch-to-buffer response-buffer)
        (let ((json (progn
                      (set-buffer-multibyte t)
                      (goto-char url-http-end-of-headers)
                      (json-read))))
          (with-current-buffer (get-buffer-create "*lingva*")
            (let ((inhibit-read-only t))
              (special-mode)
              (delete-region (point-min) (point-max))
              (insert (cdar json))
              (kill-new (cdar json))
              (message "Translation copied to clipboard.")
              (display-buffer (current-buffer)))))))))

;; process the http response:

(defun lingva--response ()
  "Capture response buffer content as string."
  (with-current-buffer (current-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lingva--response-body (pattern)
  "Return substring matching PATTERN from `lingva--response'."
  (let ((resp (lingva--response)))
    (string-match pattern resp)
    (match-string 0 resp)))

(defun lingva--status ()
  "Return HTTP Response Status Code from `lingva--response'."
  (let* ((status-line (lingva--response-body "^HTTP/1.*$")))
    (string-match "[0-9][0-9][0-9]" status-line)
    (match-string 0 status-line)))

(provide 'lingva)
;;; lingva.el ends here

