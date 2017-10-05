;; -*- eval: (git-auto-commit-mode 1) -*-
;;
(require 'org)
(require 'org-contacts)
(require 'org-bullets)
(require 'ox-bibtex)
(require 'ox-extra)
(require 'org-inlinetask)

(provide 'org-config)

;;; Fix for org meta return
;; https://github.com/syl20bnr/spacemacs/issues/9603
(org-defkey org-mode-map [(meta return)] 'org-meta-return)

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

;;; Publishing
;; TODO could DRY up a bit
(setq org-publish-project-alist
      '(("cv-normal"
         :base-directory "~/Bitbucket/cv/"
         :publishing-directory "~/Desktop/out/normal"
         ;; :publishing-function org-html-publish-to-html
         :base-extension "org"
         ;; :exclude-tags ":noexport:extended:"
         :include "TODOs.org"
         :section-numbers nil
         :with-toc nil)
        ;; ("cv-extended"
        ;;  :base-directory "~/Bitbucket/cv/"
        ;;  :publishing-directory "~/Bitbucket/cv/out/extended"
        ;;  :publishing-function org-html-publish-to-html
        ;;  :include "TODOs.org"
        ;;  :section-numbers nil
        ;;  :with-toc nil)
        ;; ("cv" :components ("cv-normal" "cv-extended"))
        ))


;;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages '((python .  t)
                             (haskell . t)
                             (clojure . t)
                             (dot .     t)
                             (shell .     t)
                             (latex .     t)
                             (js . t)
                             (browser . t)))


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
