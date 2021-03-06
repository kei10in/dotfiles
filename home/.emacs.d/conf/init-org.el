;;; init-org.el --- OrgMode Configurations

;; Copyright (C) 2011  Keisuke Ijuin

;; Author: Keisuke Ijuin <kei10in@gmail.com>
;; Keywords: tools, docs

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

(require 'org)

(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '("TODO(t)" "WAITING(w)" "TODAY(y)" "DOING(d)" "|" "DONE(x)" "CANCEL(c)"))
(setq org-agenda-files '("~/Documents/Work/OrgNote/index.org"))

(setq org-log-done t)

;; Org Mode Keybinds
(define-key org-mode-map (kbd "<tab>") 'org-metaright)
(define-key org-mode-map (kbd "<S-tab>") 'org-metaleft)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)

(provide 'init-org)
;;; init-org.el ends here
