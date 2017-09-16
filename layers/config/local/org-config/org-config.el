(require 'org)
(require 'org-contacts)
(require 'org-bullets)
(require 'ox-bibtex)
(require 'ox-extra)

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

;; Quick refile of project tasks
(setq org-refile-targets '((nil :regexp . "Week of")))

(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)

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

(setq
 org-structure-template-alist
 '(("n" "#+NAME: ?")
   ("q" "#+BEGIN_QUOTE\n\n#+END_QUOTE")

   ;; Language Blocks
   ("c" "#+BEGIN_SRC clojure\n\n#+END_SRC")
   ("d" "#+BEGIN_SRC dot\n\n#+END_SRC")
   ("e" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
   ("h" "#+BEGIN_SRC haskell\n\n#+END_SRC")
   ("l" "#+BEGIN_SRC lisp\n\n#+END_SRC")
   ("p" "#+BEGIN_SRC python\n\n#+END_SRC")
   ("s" "#+BEGIN_SRC shell\n\n#+END_SRC")
   ("j" "#+BEGIN_SRC js :results replace\n\n#+END_SRC")

   ;; Collapse previous header by default in themed html export
   ("clps" ":PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")
   ;; Hugo title template
   ("b" "#+TITLE: \n#+SLUG: \n#+DATE: 2017-mm-dd
#+CATEGORIES: \n#+SUMMARY: \n#+DRAFT: false")))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Bitbucket/org/gtd.org" "Tasks")
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
(setq org-agenda-files (list (os-path "~/Bitbucket/org/code.org")))

(when is-linuxp
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                        ("\\.pdf\\'" . default))))

;; TODO reenable
;; (add-to-list 'org-latex-packages-alist '("" "minted"))

;; (setq org-latex-listings 'minted)
;; (setq org-latex-minted-options '(("frame" "lines")
;;                                  ("fontsize" "\\scriptsize")
;;                                  ("xleftmargin" "\\parindent")
;;                                  ("linenos" "")))


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
                             (shell .   t)
                             (latex .   t)
                             (js . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-window-setup 'current-window)

;; TODO setup these correctly
;; (setq org-babel-default-header-args:python
;;       (cons '(:results . "output file replace")
;;             (assq-delete-all :results org-babel-default-header-args)))
