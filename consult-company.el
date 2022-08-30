;;; consult-company.el --- Consult frontend for company -*- lexical-binding: t; -*-

;; Copyright (C) 2021  mohsin kaleem

;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Package-Requires: ((emacs "27.1") (company "0.9") (consult "0.9"))
;; Version: 0.2
;; URL: https://github.com/mohkale/consult-company

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

;; This package provides a command to interactively complete a company
;; completion candidate through completing-read using the consult API.
;; This works much like the builtin `completion-at-point' command except
;; it can accept candidates from `company-backends' making it consistent
;; with the completion candidates you would see in the company popup.

;;; Code:

(require 'company)
(require 'consult)

(defgroup consult-company nil
  "Consult frontend for company."
  :group 'company
  :group 'consult)

(defcustom consult-company-narrow
  `((color ?# . "Color")
    ((file folder) ?f . "Files")
    (text ?w . "Word")
    (snippet ?s . "Snippet")
    (t ?c . "Completion"))
  "Narrow configuration for `consult-company'.
Each entry should be a list beginning with either a symbol or list
of symbols with the same keys as `company-text-icons-mapping' and
ending with a cons cell of the `consult' narrow key and title for
those company-kinds."
  :type '(list (choice (symbol :tag "Completion kind")
                       (repeat (symbol :tag "Completion kind")))
               (cons (character :tag "Narrow key")
                     (string :tag "Display name"))))

(defcustom consult-company-group-by-kind t
  "Group completion candidates in the minibuffer by `consult-company-narrow'.
This won't affect the actual narrowing functionality, just the presentation
of completion candidates. Thus even when this is nil you can still narrow to
a completion-kind with the configured keys."
  :type 'boolean)

(defcustom consult-company-preview-function nil
  "Preview function to use for `consult-company'.
This should be a function accepting the source buffer where
the completion candidates were generated and returning a state
callback as expected by `consult'."
  :type '(choice
          (const :tag "Use location of completion candidate"
                 consult-company-preview-location)
          (const :tag "Use documentation associated with candidate"
                 consult-company-preview-doc-buffer)
          function
          (const :tag "No state function" nil)))



(defun consult-company--candidates ()
  "Retrieve a list of candidates for `consult-company'."
  (cl-loop for cand in company-candidates
           ;; We need to keep the original candidate separate from the one passed
           ;; to completing-read to prevent the useful text-properties from being
           ;; stripped.
           with ix = 0 do (setq ix (1+ ix))
           with cand-str = nil
           do (setq cand-str (consult--tofu-append (substring-no-properties cand) ix))
           ;; Assign the backend for candidate, defaults to capf for unknown backends.
           with backend = nil
           do (setq backend
                    (or (get-text-property 0 'company-backend cand)
                        (if (consp company-backend)
                            (car (cl-remove-if #'symbolp company-backend))
                          company-backend)))
           ;; Apply completions-common face to non-capf candidates.
           when (and (not (eq backend 'company-capf))
                     (string-prefix-p company-prefix cand))
             ;; Copy candidate and prefix with completions-common-part
             do (add-face-text-property 0 (length company-prefix)
                                          'completions-common-part nil cand-str)
           ;; Assign the consult--type property for title and narrowing.
           do (when-let ((narrow (or (when-let ((company-backend backend)
                                                (kind (company-call-backend 'kind cand)))
                                       (cl-assoc kind consult-company-narrow
                                                 :test (lambda (it x)
                                                         (or (eq x it)
                                                             (and (listp x)
                                                                  (member it x))))))
                                   (assoc t consult-company-narrow))))
              (add-text-properties 0 1 (list 'consult--type (cadr narrow)) cand-str))
           collect (cons cand-str cand)))

(defun consult-company-preview-location (orig-buffer)
  "`consult-company' preview function showing completion location.
This preview function will make `consult-company' show the definition
of each completion candidate as its selected in the minibuffer. This
requires the completion backend which provides this completion to
support the 'location request.

ORIG-BUFFER is the source buffer where the completion candidates were
generated."
  (let ((preview (consult--jump-preview)))
    (lambda (action cand)
      (let* ((location (with-current-buffer orig-buffer
                         (company-call-backend 'location cand)))
             (location (when location
                         (let ((marker (make-marker)))
                           (set-marker marker (cdr location) (car location))
                           marker))))
        (funcall preview action location)
        (when (and location (eq action 'return))
          (consult--jump location))))))

(defun consult-company-preview-doc-buffer (orig-buffer)
  "`consult-company' preview function showing completion documentation.
This preview function will make `consult-company' show the documentation
of each completion candidate as its selected in the minibuffer. This
requires the completion backend which provides this completion to
support the 'doc-buffer request.

ORIG-BUFFER is the source buffer where the completion candidates were
generated."
  (let ((preview (consult--buffer-preview)))
    (lambda (action cand)
      (let* ((doc-buffer (with-current-buffer orig-buffer
                           (company-call-backend 'doc-buffer cand))))
        (when (consp doc-buffer)
          (setq ;; start (cdr doc-buffer)
                doc-buffer (car doc-buffer)))

        (funcall preview action doc-buffer)
        (when (and cand (eq action 'return))
          (consult--buffer-action doc-buffer))))))

(defun consult-company--read ()
  "Read a company candidate."
  (unless company-candidates
    (company-complete))
  (let ((cands (consult--with-increased-gc
                (consult-company--candidates)))
        (narrow-assoc (mapcar #'cdr consult-company-narrow))
        (original-buffer (current-buffer)))
    (unless cands
      (user-error "No completion candidates available"))
    (consult--read
     cands
     :prompt "Candidate: "
     :lookup #'consult--lookup-cdr
     :sort nil
     :category 'consult-company
     :group
     (when consult-company-group-by-kind
       (consult--type-group narrow-assoc))
     :narrow
     (let* ((narrow (consult--type-narrow narrow-assoc))
            (pred (plist-get narrow :predicate)))
       `(:predicate
         ,(lambda (cand)
            (funcall pred (car cand)))
         ,@narrow))
     :annotate
     (lambda (cand)
       (when-let* ((company-cand (consult--lookup-cdr cand cands))
                   (annotation
                    (with-current-buffer original-buffer
                      (company-call-backend 'annotation company-cand)))
                   (annotation (company--clean-string annotation)))
         (unless (string-empty-p annotation)
           (concat
            (propertize " " 'display `(space :align-to (- right 1 ,(length annotation))))
            (propertize annotation 'face 'completions-annotations)))))
     :state
     (when consult-company-preview-function
       (funcall consult-company-preview-function original-buffer)))))

;;;###autoload
(defun consult-company (cand)
  "Interactively complete a company candidate.
CAND should be an entry in `company-candidates'."
  (interactive
   (list (consult-company--read)))
  (company-finish cand))

(provide 'consult-company)
;;; consult-company.el ends here
