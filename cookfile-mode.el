;;; cookfile-mode.el --- Support for editing dockercook files
;; Copyright (C) 2014-2015 factis research GmbH
;;
;; Author: David Leuschner <leuschner@factisresearch.com>
;; Created: 2014-08-19
;; Version: 0.0.3
;;
;; This file is not part of GNU Emacs.
;;
;; This software is released under the MIT license.
(require 'regexp-opt)

(defvar cook-keywords '("INCLUDE" "UNPACK" "BASE COOK" "BASE DOCKER" "RUN" "CMD" "EXPOSE" "ENV"
                        "ADD" "ENTRYPOINT" "VOLUME" "USER" "WORKDIR" "COPY" "PREPARE" "SCRIPT"
                        "BEGIN" "COMMIT" "DOWNLOAD" "COOKCOPY" "COOKVAR"))

(defvar cook-keywords-regexp
  (concat "^\\s-*" (regexp-opt cook-keywords 'words)))

(defun cook-font-lock-keywords ()
  `((,cook-keywords-regexp . font-lock-keyword-face)))

(define-derived-mode cookfile-mode sh-mode "dockercook"
  "Major mode for editing dockercook files"
  (setq font-lock-defaults '((cook-font-lock-keywords))))

(provide 'cookfile-mode)
