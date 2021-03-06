;;; -*- mode: emacs-lisp; coding: utf-8 -*-

;;; init-scala.el --- Scala major mode

;; Copyright (C) 2010  Keisuke Ijuin

;; Author: Keisuke Ijuin <kei10in@gmail.com>
;; Keywords: emacs.el scala-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Scala-mode
;; http://www.scala-lang.org/

;; ENSIME
;; https://github.com/aemoncannon/ensime

;;; Code:

(require 'init-package)

;;; Scala-mode
(require 'scala-mode2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(scala-indent:align-parameters t)
 ;; '(scala-indent:indent-value-expression t)
)

(require 'utils)

(add-hook
 'scala-mode-hook
 '(lambda ()
    (local-set-key (kbd "RET")
                   'newline-skeleton-pair-insert-brace)
    (local-set-key (kbd "<backtab>")
                   'scala-indent:indent-with-reluctant-strategy)
    ))


;;; ENSIME
(defvar ensime-dir
  (concat private-lisp-dir "./ensime/elisp/"))
(add-to-list 'load-path
			 (expand-file-name ensime-dir))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


(provide 'init-scala)
;;; init-scala.el ends here
