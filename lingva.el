;;; lingva.el --- access google translate via lingva.ml  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  mouse

;; Author: mouse <mousebot@riseup.net>
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

(defcustom lingva-instance "https://lingva.ml"
  "The lingva instance to use.")

(defvar lingva-languages
  '(("bn" . "Bengali")
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
   ("zu" . "Zulu"))
  "The list of languages to choose from as either source or
  target for a lingva query.")

;; let user choose these from lingva-languages
(defcustom lingva-source "fr"
  "The default language to translate from, as a two letter code.
For details of what languages are availble and their
corresponding codes, see `lingva-languages'.")

(defcustom lingva-target "en"
  "The default language to translate to, as a two letter code.
For details of what languages are availble and their
corresponding codes, see `lingva-languages'.")

(defvar lingva-search-url
  (concat lingva-instance "/api/v1/" lingva-source "/" lingva-target "/")
  "The URL for a lingva query.")

(defvar lingva-languages-url
  (concat lingva-instance "/api/v1/languages")
  "The URL for a lingva source and target languages list query.")

;; if don't want hard-coded linva-languages list, ask the server:
;; (defun lingva-get-languages ()
;;   (let* ((url-request-method "GET")
;;          (response-buffer (url-retrieve-synchronously
;;                            lingva-languages-url)))
;;     (setq lingva-languages
;;           (with-current-buffer response-buffer
;;             (goto-char (point-min))
;;             (search-forward "\n\n")
;;             (json-read)))))

;;;###autoload
(defun lingva-translate (text &optional arg)
  "Prompt for TEXT to translate and return the translation in a buffer.
With a single prefix ARG, prompt to specify a source
language different to `lingva-source'.
With a second prefix ARG, prompto to specify both a source
language different to `lingva-source' and a target language
different to `lingva-target'."
  (interactive "sText to translate: \nP")
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
         (response-buffer (url-retrieve-synchronously
                           (concat lingva-instance "/api/v1/"
                                   lingva-source "/"
                                   lingva-target "/"
                                   text)))
         (json
          (with-current-buffer response-buffer
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
        (display-buffer (current-buffer))))))

(provide 'lingva)
;;; lingva.el ends here

