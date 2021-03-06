;;; -*- mode: emacs-lisp; coding: utf-8 -*-

;;; init-package.el --- package management configuraitons

;; Copyright (C) 2010  Keisuke Ijuin

;; Author: Keisuke Ijuin <kei10in@gmail.com>
;; Keywords: elisp tools

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

;; Auto Install
;; http://www.emacswiki.org/AutoInstall
;; (install-elisp-from-emacswiki "auto-install.el")

;;; Code:

;;; Cask
(require 'cask "~/.cask/cask.el")
(setq user_bundle (cask-initialize "~/.emacs.d"))
(cask-install user_bundle)
(cask-update  user_bundle)

(require 'pallet)

;;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/site-lisp/")
(add-to-list 'load-path auto-install-directory)

;; when non nil, add emacs wiki page name to completion candidates 
(auto-install-update-emacswiki-package-name nil)

;; compatible install-elisp.el
(auto-install-compatibility-setup)

(provide 'init-package)
;;; init-package.el ends here
