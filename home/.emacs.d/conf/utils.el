;;; utils.el --- some utils for emacs configurations

;; Copyright (C) 2013  Keisuke Ijuin

;; Author: Keisuke Ijuin <kei10in@gmail.com>
;; Keywords: lisp

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

;; 

;;; Code:

(defun newline-skeleton-pair-insert-brace ()
  (interactive)
  (if (and (eq (char-before) ?{)
           (or (eq last-command 'yank)
               (eq last-command 'self-insert-command)
               (eq last-command 'c-electric-brace)))
      (progn
        (newline-and-indent)
        (save-excursion (insert "\n}") (indent-according-to-mode)))
    (newline-and-indent)))


(provide 'utils)
;;; utils.el ends here
