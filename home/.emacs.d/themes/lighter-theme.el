;;; lighter-theme.el --- Basic theme lighter than default theme.

;; Copyright (C) 2012  Keisuke Ijuin

;; Author: Keisuke Ijuin <kei10in@gmail.com>
;; Keywords: faces

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

(deftheme lighter
  "Face colors using lighter color than default theme.")

(custom-theme-set-faces
 'lighter
 '(default ((t (:background "white" :foreground "black"))))
 '(cursor ((t (:background "#999999"))))
 '(highlight ((t (:background "LightSteelBlue1"))))
 '(hl-line ((t (:background "LightSteelBlue1"))))
 '(flymake-errline ((t (:background "LightPink"))))
 '(flymake-warnline ((t (:background "LightBlue2"))))
 ;; Org Mode face
 '(org-level-1 ((t (:foreground "Blue1"))))
 '(org-level-2 ((t (:foreground "DarkGoldenrod"))))
 '(org-level-3 ((t (:foreground "Purple"))))
 '(org-level-4 ((t (:foreground "Firebrick"))))
 '(org-level-5 ((t (:foreground "ForestGreen"))))
 '(org-level-6 ((t (:foreground "CadetBlue"))))
 '(org-level-7 ((t (:foreground "Orchid"))))
 '(org-level-8 ((t (:foreground "RosyBrown"))))
 ;; helm mode face
 '(helm-buffer-directory ((t (:foreground "Blue1" :background "white"))))
 '(helm-ff-directory ((t (:foreground "Blue1" :background "white"))))
 '(helm-ff-file ((t (:foreground "black"))))
 '(helm-selection ((t (:background "LightSteelBlue1" :underline nil))))
 '(helm-visible-mark ((t (:inherit dired-flagged))))
 ;; auto-completion mode face
 '(ac-completion-face ((t (:background "blue" :foreground "*" :underline t))))
 ;; grap
 '(grep-edit-face ((t (:background "#442222"))))
 ;; rst mode
 '(rst-level-1-face ((t (:foreground "Blue1" :background "gray85"))))
 '(rst-level-2-face ((t (:foreground "DarkGoldenrod" :background "gray78"))))
 '(rst-level-3-face ((t (:foreground "Purple" :background "gray71"))))
 '(rst-level-4-face ((t (:foreground "Firebrick" :background "gray64"))))
 '(rst-level-5-face ((t (:foreground "ForestGreen" :background "gray57"))))
 '(rst-level-6-face ((t (:foreground "CadetBlue" :background "gray50"))))
)

(provide-theme 'lighter)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lighter-theme.el ends here
