;; -*- eval: (git-auto-commit-mode 1) -*-
;;


(require 'org)
(require 'org-contacts)
(require 'org-bullets)
(require 'ox-bibtex)
(require 'ox-extra)
(require 'org-inlinetask)

(provide 'org-config)

;;; Bindings and Hooks

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-toggle-blocks)

(spacemacs/set-leader-keys "aof" 'org-open-at-point-global)

(define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

(evil-define-key '(normal visual motion) org-mode-map
  "gh" 'outline-up-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "gl" 'outline-next-visible-heading
  "gu" 'outline-previous-visible-heading)

;; Refiling
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((nil :maxlevel . 9)
                           ("~/Bitbucket/org/someday.org" :maxlevel . 1)
                           ("~/Bitbucket/org/tickler.org" :maxlevel . 1)
                           (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)
;;

(defun org-config-export-remove-prelim-headlines (tree backend info)
  "Remove headlines tagged \"prelim\" while retaining their
contents before any export processing."
  (org-element-map tree org-element-all-elements
    (lambda (object)
      (when (and (equal 'headline (org-element-type object))
                 (member "prelim" (org-element-property :tags object)))
        (mapc (lambda (el)
                ;; recursively promote all nested headlines
                (org-element-map el 'headline
                  (lambda (el)
                    (when (equal 'headline (org-element-type el))
                      (org-element-put-property el
                                                :level (1- (org-element-property :level el))))))
                (org-element-insert-before el object))
              (cddr object))
        (org-element-extract-element object)))
    info nil org-element-all-elements)
  tree)

(add-hook 'org-export-filter-parse-tree-functions
           'org-config-export-remove-prelim-headlines)

;;; Theming

(setq org-priority-faces '((65 :inherit org-priority :foreground "red")
                           (66 :inherit org-priority :foreground "brown")
                           (67 :inherit org-priority :foreground "blue")))
(setq org-ellipsis "")
(setq org-bullets-bullet-list '("" "" "" ""))

;;; Templates

;; (setq
;;  org-structure-template-alist
;;  '(("n" "#+NAME: ?")
;;    ("q" "#+BEGIN_QUOTE\n\n#+END_QUOTE")

;;    ;; Language Blocks
;;    ("c" "#+BEGIN_SRC clojure\n\n#+END_SRC")
;;    ("d" "#+BEGIN_SRC dot\n\n#+END_SRC")
;;    ("e" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
;;    ("h" "#+BEGIN_SRC haskell\n\n#+END_SRC")
;;    ("l" "#+BEGIN_SRC lisp\n\n#+END_SRC")
;;    ("p" "#+BEGIN_SRC python\n\n#+END_SRC")
;;    ("s" "#+BEGIN_SRC shell\n\n#+END_SRC")
;;    ("j" "#+BEGIN_SRC js :results replace\n\n#+END_SRC")

;;    ;; Collapse previous header by default in themed html export
;;    ("clps" ":PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")
;;    ;; Hugo title template
;;    ("b" "#+TITLE: \n#+SLUG: \n#+DATE: 2017-mm-dd
;; #+CATEGORIES: \n#+SUMMARY: \n#+DRAFT: false")))

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Bitbucket/org/inbox.org")
         "* TODO %?\n  %i\n  %a")
        ("c" "Code journal" entry (file+olp+datetree "~/Bitbucket/org/code.org" "Journal")
         "* %?\nEntered on %U\n  %i\n  %a"
         :jump-to-captured t)))

;;; Org Blocks

;; Hide all org-blocks, including src, quote, etc. blocks, on buffer load
(defvar org-blocks-hidden nil)
(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))

;;; Export

(ox-extras-activate '(ignore-headlines))

;; (setq org-contacts-files (list (os-path "~/Dropbox/contacts.org")))
(setq org-agenda-files (list (os-path "~/Bitbucket/org/code.org")
                             (os-path "~/Bitbucket/org/gtd.org")))

;; TODO Move elsewhere and setup correctly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-agenda-custom-commands
      '(("c" "At the computer" tags-todo "@computer"
         ((org-agenda-overriding-header "Computer")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when is-linuxp
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                        ("\\.pdf\\'" . default))))

;; (add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-latex-listings 'minted)
(setq org-latex-minted-options '(("frame" "lines")
                                 ("fontsize" "\\scriptsize")
                                 ("xleftmargin" "\\parindent")
                                 ("linenos" "")))


(add-to-list 'org-latex-packages-alist
             '("" "tikz" t))

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(setq org-latex-create-formula-image-program 'imagemagick)

;; (setq
;;  org-latex-pdf-process
;;  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; TODO configure from here: http://matt.hackinghistory.ca/2015/11/11/note-taking-with-pdf-tools/
;; (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
;; (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))

;;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages '((python .  t)
                             (haskell . t)
                             (clojure . t)
                             (dot .     t)
                             (shell .     t)
                             (latex .     t)
                             (js . t)))


(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-window-setup 'current-window)

(setq org-babel-js-function-wrapper "\n%s\n")

(setq org-babel-default-header-args
      '((:session . "none")
        (:exports . "code")
        (:results . "replace")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")))

;; Temp override org-babel-eval to output errors as results
;; Orginal function is here: https://github.com/emacsmirror/org/blob/master/lisp/ob-eval.el
(defun org-babel-eval (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
  (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
	          (org-babel--shell-command-on-region
	           (point-min) (point-max) cmd err-buff))
      (buffer-string))))

;;; Org Return
;; From here: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/

(defun org-config-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return-indent))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
          (bolp))
      (org-return))

     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))

     ;; checkboxes too
     ((org-at-item-checkbox-p)
      (org-insert-todo-heading nil))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
          (org-insert-heading)
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position))
        (org-return)))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading-respect-content)
                 (outline-show-entry))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))

     ;; fall-through case
     (t
      (org-return)))))

;; Disable for now
;; (define-key org-mode-map (kbd "RET")
;;   'org-config-return)
