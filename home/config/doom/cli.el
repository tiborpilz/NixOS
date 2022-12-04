;;; cli.el -*- lexical-binding: t; -*-
(setq org-configm-babel-evaluate nil)

(defun doom-shut-up-a (orign-fn &rest args)
  (quiet! (apply origin-fn args)))

(advice-add 'org-babel-execute-src-block :around #'doom-shut-up-a)

;; Don't compile packages on install
(advice-add #'native-compile-async :override #'ignore)
