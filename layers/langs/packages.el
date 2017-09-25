;;; Langs Packages

(defconst langs-packages
      '(
        lispy
        virtualenvwrapper

        (python :location built-in)

        ;; (mypy-flycheck :location local)
        (windows-pytest :location local)

        ;; JS stuff
        add-node-modules-path
        company-flow
        eslintd-fix
        flycheck
        prettier-js
        rjsx-mode))

;;; Lispy

(defun langs/init-lispy ()
  (use-package lispy
    :config (require 'le-hy)))

;;; Virtualenvwrapper

(defun langs/init-virtualenvwrapper ()
  (use-package virtualenvwrapper
    :after python
    :config
    (progn
      (pyvenv-mode 1)
      (venv-initialize-interactive-shells)
      (venv-initialize-eshell)

      ;; Fixes hy-mode environment when pyvenv is activated
      (add-hook 'pyvenv-post-activate-hooks 'python/init-hy-mode))))

;;; Python

(defun langs/post-init-python ()
  ;; Sometimes ipython shells trigger a bad error to popup
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_")))

  ;; Remove flyspell
  (add-hook 'python-mode-hook (lambda () (flyspell-mode -1)))

  ;; Whether to print logs in pytest
  ;; (setq pytest-cmd-flags "-x --no-print-logs")

  ;; Enables python shell to print unicode
  (setenv "PYTHONIOENCODING" "utf-8")
  (setenv "LANG" "en_US.UTF-8")

  ;; Cython numpy symlink
  (setenv "CFLAGS" "-I /root/.virtualenvs/pop-synthvenv/lib/python3.6/site-packages/numpy/core/include"))

;;; Windows-pytest

(defun langs/init-windows-pytest ()
  (use-package windows-pytest
    :after python
    :if (not is-linuxp)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "tm" 'ek-pytest-module
      "tt" 'ek-pytest-one)))

;;; Javascript

(defun langs/init-eslintd-fix ()
  (use-package eslintd-fix
    :defer t
    :commands eslintd-fix-mode
    :init
    (progn
      (add-hook 'rjsx-mode-hook #'eslintd-fix-mode t))))

(defun langs/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

      (setq
       js2-mode-show-strict-warnings nil
       js2-mode-show-parse-errors nil
       js-indent-level 2
       js2-basic-offset 2
       js2-strict-trailing-comma-warning nil
       js2-strict-missing-semi-warning nil)

      (advice-add #'js-jsx-indent-line
                  :after
                  #'langs/js-jsx-indent-line-align-closing-bracket)
      (add-hook 'rjsx-mode-hook #'langs/eslintd-set-flycheck-executable t))
    :config
    (modify-syntax-entry ?_ "w" js2-mode-syntax-table)))

(defun langs/post-init-add-node-modules-path ()
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'add-node-modules-path)))

(defun langs/post-init-company-flow ()
  (spacemacs|add-company-backends
    :backends
    '((company-flow :with company-dabbrev-code)
      company-files)))

(defun langs/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))

  (spacemacs/enable-flycheck 'rjsx-mode))

(defun langs/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :init
    (progn
      (add-hook 'rjsx-mode-hook 'prettier-js-mode)
      (setq prettier-js-args '(
                               "--trailing-comma" "es5"
                               "--bracket-spacing" "false"
                               ;; "--no-semi"
                               "--single-quote")))))
