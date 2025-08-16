;;; early-init.el --- Early initialization file -*- lexical-binding: t -*-
;;; Commentary:
;;; This file is loaded before the package manager and before the GUI is initialized.

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;;; early-init.el ends here
