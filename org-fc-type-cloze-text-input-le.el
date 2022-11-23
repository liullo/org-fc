;;; org-fc-type-cloze-text-inputt-le.el --- Cloze deletion card type -*- lexical-binding: t; -*-

;;; Code:

(require 'org-fc-core)

(defcustom org-fc-type-cloze-text-input-le-type-property "FC_CLOZE_TYPE"
  "Property used to store the card's subtype for cloze cards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-type-cloze-text-input-le-context 1
  "Number of surrounding cards to show for 'context' type cards."
  :type 'number
  :group 'org-fc)

(defface org-fc-type-cloze-text-input-le-hole-face
  '((t (:bold t :foreground "red")))
  "Face for org-fc cloze card holes."
  :group 'org-fc)

(defcustom org-fc-type-cloze-text-input-le-hint-prefix
  "..."
  "Prefix for cloze hints."
  :group 'org-fc)

(defvar org-fc-type-cloze-text-input-le-types
  '(deletion enumeration context single)
  "List of valid cloze card subtypes.")

(defvar org-fc-type-cloze-text-input-le--text '()
  "Text overlay.")
(defvar org-fc-type-cloze-text-input-le--hint '()
  "Hint overlay.")

;;; Hole Parsing / Hiding

(defvar org-fc-type-cloze-text-input-le-hole-re
  (rx
   (seq
    "{{"
    (group-n 1 (* (or (seq "$" (+ (not (any "$"))) "$")
                      (not (any "}"))))) "}"
    (? (seq "{" (group-n 2 (* (or (seq "$" (not (any "$")) "$")
                                  (not (any "}"))))) "}"))
    (? "@" (group-n 3 (+ digit)))
    "}"))
  "Regexp for a cloze holes.")

(defun org-fc-type-cloze-text-input-le-max-hole-id ()
  "Get the max-hole property of the heading at point."
  (if-let ((max-id (org-entry-get (point) org-fc-type-cloze-max-hole-property)))
      (string-to-number max-id)
    -1))

(defun org-fc-type-cloze-text-input-le--parse-holes (current-position end)
  "Starting at point, collect all cloze holes before END.
CURRENT-POSITION is the id of the hole being reviewed.  Returns a
pair (holes . current-index) where current-index is the index of
the hole for the current position."
  (let (holes current-index)
    (while (re-search-forward org-fc-type-cloze-text-input-le-hole-re end t)
      (when (match-beginning 3)
        (push (match-data) holes)
        (if (= current-position (string-to-number (match-string 3)))
            (setq current-index (1- (length holes))))))
    (cons (reverse holes) current-index)))

(defun org-fc-type-cloze-text-input-le--hole-visible-p (type i current-index)
  "Determine whether hole I of card TYPE should be visible based.
CURRENT-INDEX is the index of the current position in the list of all holes."
  (cl-case type
    ('enumeration (< i current-index))
    ('deletion t)
    ('single nil)
    ('context (<= (abs (- i current-index)) org-fc-type-cloze-text-input-le-context))
    (t (error "Org-fc: Unknown cloze card type %s" type))))

(defun org-fc-type-cloze-text-input-le--end ()
  "End of contents of heading at point, excluding subheadings."
  (save-excursion
    ;; If there is no next heading, we end up at `(point-max)`
    (outline-next-heading)
    (1- (point))))

(defun org-fc-type-cloze-text-input-le-hide-holes (position)
  "HHAH Hide holes of a card of TYPE in relation to POSITION."
  (org-fc-with-point-at-entry
   (let* ((type (intern (org-entry-get (point) org-fc-type-cloze-text-input-le-type-property)))
          (end (1+ (org-fc-type-cloze-text-input-le--end)))
          (holes-index (org-fc-type-cloze-text-input-le--parse-holes position end))
          (holes (car holes-index))
          (current-index (cdr holes-index)))
     (cl-loop
      for i below (length holes)
      for (hole-beg hole-end text-beg text-end hint-beg hint-end) in holes
      do
      (progn
        ;; Fake position if there is no hint
        (unless hint-beg (setq hint-beg text-end))
        (unless hint-end (setq hint-end text-end))
        (cond
         ;; If the hole is the one currently being reviewed, hide all
         ;; the hole markup, hide the answer, format the hint as
         ;; "[...hint]" and set the font for the whole hole.
         ((= i current-index)
          (org-fc-hide-region hole-beg text-beg "")
          (remove-overlays text-beg text-end)
          (setq org-fc-type-cloze-text-input-le--text
                (org-fc-make-overlay text-beg text-end 'invisible t))
          (org-fc-hide-region text-end hint-beg "")
          (setq org-fc-type-cloze-text-input-le--hint
                (org-fc-overlay-surround
                 (org-fc-make-overlay hint-beg hint-end)
                 (concat "[" org-fc-type-cloze-text-input-le-hint-prefix)
                 "]"
                 'org-fc-type-cloze-text-input-le-hole-face))
          (org-fc-hide-region hint-end hole-end "")
          (org-fc-make-overlay
           hole-beg hole-end
           'face 'org-fc-type-cloze-text-input-le-hole-face)

          ;; New added code
          (let* ((text (buffer-substring-no-properties text-beg text-end))
                 (diff (org-fc-diff (read-string "Answer: ") text)))

            (save-excursion
              (goto-char (point-max))
              (org-fc-hide-region (1- (point)) (point) (concat "\n\n" (car diff) "\n" (cdr diff))))
            )
          )

         ;; If the text of another hole should be visible,
         ;; hide the hole markup and the hint
         ((org-fc-type-cloze-text-input-le--hole-visible-p type i current-index)
          (org-fc-hide-region hole-beg text-beg)
          (org-fc-hide-region text-end hole-end))
         ;; If the text of another hole should not be visible,
         ;; hide the whole hole
         (t (org-fc-hide-region hole-beg hole-end "..."))

         ))))))

;;; Setup / Flipping

(defun org-fc-type-cloze-text-input-le-init (type)
  "Initialize the current heading for use as a cloze card of subtype TYPE.
Processes all holes in the card text."
  (interactive (list
                (intern
                 (completing-read "Cloze Type: " org-fc-type-cloze-text-input-le-types))))
  (unless (member type org-fc-type-cloze-text-input-le-types)
    (error "Invalid cloze card type: %s" type))
  (org-fc--init-card "cloze_input")
  (org-fc-type-cloze-text-input-le-update)
  (org-set-property org-fc-type-cloze-text-input-le-type-property (format "%s" type)))

(defun org-fc-type-cloze-text-input-le-setup (position)
  "Prepare POSITION of a cloze card for review."
  (setq org-fc-type-cloze-text-input-le--text nil)
  (setq org-fc-type-cloze-text-input-le--hint nil)
  (outline-hide-subtree)
  (org-show-entry)
  (org-fc-type-cloze-text-input-le-hide-holes (string-to-number position)))

(defun org-fc-type-cloze-text-input-le-flip ()
  "Flip a cloze card."
  (org-show-children)
  (overlay-put org-fc-type-cloze-text-input-le--text 'invisible nil)
  (org-fc-show-latex)
  ;; Remove all overlays in the region of the hint to get rid of
  ;; latex overlays in the hint, then hide the region again.
  (let* ((hint-start (overlay-start org-fc-type-cloze-text-input-le--hint))
         (hint-end (overlay-end org-fc-type-cloze-text-input-le--hint)))
    (remove-overlays hint-start hint-end)
    (org-fc-hide-region hint-start hint-end)))

(defun org-fc-type-cloze-text-input-le-update ()
  "Update the review data & deletions of the current heading."
  (let* ((end (org-fc-type-cloze-text-input-le--end))
         (hole-id (1+ (org-fc-type-cloze-text-input-le-max-hole-id)))
         ids)
    (save-excursion
      (while (re-search-forward org-fc-type-cloze-text-input-le-hole-re end t)
        (let ((id (match-string 3))
              (hole-end (match-end 0)))
          (unless id
            (setq id hole-id)
            (cl-incf hole-id 1)
            (let ((id-str (number-to-string id)))
              (cl-incf end (+ 1 (length id-str)))
              (goto-char hole-end)
              (backward-char)
              (insert "@" id-str)))
          (push (format "%s" id) ids))))
    (org-set-property
     org-fc-type-cloze-max-hole-property
     (format "%s" (1- hole-id)))
    (org-fc-review-data-update (reverse ids))))

(org-fc-register-type
 'cloze_input
 'org-fc-type-cloze-text-input-le-setup
 'org-fc-type-cloze-text-input-le-flip
 'org-fc-type-cloze-text-input-le-update)

;;; Footer

(provide 'org-fc-type-cloze-text-input-le)

;;; org-fc-type-cloze-text-input-le.el ends here
