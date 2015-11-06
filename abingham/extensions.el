;;; extensions.el --- abingham Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq abingham-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq abingham-post-extensions
      '(
        codesearch
        ))

;; For each extension, define a function abingham/init-<extension-name>
;;
(defun abingham/init-codesearch ()
  "Initialize codesearch"
  (use-package codesearch
    :ensure t
    :bind
    (("M-'" . codesearch-search)
     ("M-." . projectile-codesearch-search))
    :config
    (set-variable 'codesearch-cindex "~/go/bin/cindex")
    (set-variable 'codesearch-csearch "~/go/bin/csearch")
    (set-variable 'codesearch-cindex-flags '())))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
