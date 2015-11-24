;;; cycle-region.el --- Region ring with preview

;; Copyright (C) 2015 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/cycle-region
;; Version: 0.0.1
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides the global minor mode `cycle-region-mode' for
;; tracking the past regions and `cycle-region-preview' to visualize
;; the region ring.

;; See the README for more info:
;; https://github.com/wasamasa/cycle-region

;;; Code:

(defgroup cycle-region nil
  "Region ring with preview."
  :group 'convenience
  :prefix "cycle-region-")


;;; Region ring

(defvar cycle-region-ring nil
  "Ring holding the previously active regions.

Each item is a list of the command responsible for deactivating
the region and the values of point and mark of that region.")
(make-variable-buffer-local 'cycle-region-ring)

(defcustom cycle-region-size 10
  "Size of the region ring."
  :type 'integer
  :group 'cycle-region)

;; NOTE The basic idea behind the following code is that it's possible
;; to detect the deactivation of the region by observing the command
;; loop before and after execution of a command.  If the region has
;; been active *before*, but is no longer active *after* the command,
;; chances are the region was deactivated by that command.  This
;; obviously requires keeping track of the previous region state.

;; FIXME Doesn't work with evil's block selection for whatever reason
;; FIXME Doesn't capture the specific type of evil's selection

(defvar cycle-region-state nil
  "Holds the region state.
Non-nil if the region is active.")

(defun cycle-region-pre-command ()
  "Tracks the region state before command execution."
  (setq cycle-region-state (region-active-p)))

(defun cycle-region--empty-region-p ()
  "Non-nil if the region is empty."
  (= (point) (mark)))

(defun cycle-region--same-region-p (index)
  "Non-nil if the region is the same as the one at INDEX."
  (when (> (ring-length cycle-region-ring) 0)
    (let* ((region (ring-ref cycle-region-ring index))
           (point (car region))
           (mark (cdr region)))
      (or (and (= point (point)) (= mark (mark)))
          ;; Point and mark can be swapped
          (and (= point (mark)) (= mark (point)))))))

(defun cycle-region-post-command ()
  "Tracks whether the region got deactivated."
  ;; The mark can be nil initially
  (when (and cycle-region-state (not (region-active-p)) (mark))
    (when (not cycle-region-ring)
      (setq cycle-region-ring (make-ring cycle-region-size)))
    (when (and (not (cycle-region--empty-region-p))
               (not (cycle-region--same-region-p 0))
               (not (cycle-region--same-region-p -1)))
      (ring-insert cycle-region-ring (cons (point) (mark))))))

;;;###autoload
(define-minor-mode cycle-region-mode
  "Toggle `cycle-region-mode'.
Tracks regions per buffer and allows you to cycle through their
history with an interactive preview."
  :keymap nil
  :global t
  (if cycle-region-mode
      (progn
        (add-hook 'pre-command-hook 'cycle-region-pre-command)
        (add-hook 'post-command-hook 'cycle-region-post-command))
    (remove-hook 'pre-command-hook 'cycle-region-pre-command)
    (remove-hook 'post-command-hook 'cycle-region-post-command)))


;;; Preview

(defface cycle-region-preview
  '((t :inherit secondary-selection))
  "Preview face for the `cycle-region' package.")

(defvar cycle-region-preview-overlay nil
  "Holds the preview overlay for a region.")

(defvar cycle-region-preview-index nil
  "Index of the currently previewed region.")

(defcustom cycle-region-pre-preview-hook nil
  "Hook run before initiating the region preview."
  :type 'hook
  :group 'cycle-region)

(defcustom cycle-region-post-preview-hook nil
  "Hook run after quitting the region preview."
  :type 'hook
  :group 'cycle-region)

(defvar cycle-region-preview-map
  (let ((map (make-sparse-keymap)))
    ;; HACK The usual way of quitting a transient map is hitting a key
    ;; not bound in it, but this will pass it through for further
    ;; lookup.  Therefore a bogus command is bound to "q" to avoid
    ;; pass-through.  The exit function does the real clean-up.
    (define-key map (kbd "q") 'ignore)
    (define-key map (kbd "RET") 'cycle-region-activate)
    (define-key map (kbd "p") 'cycle-region-backward)
    (define-key map (kbd "n") 'cycle-region-forward)
    map)
  "Keymap for the region preview.")

(defun cycle-region--preview-keep-p ()
  (memq this-command '(cycle-region-backward cycle-region-forward)))

(defcustom cycle-region-display-usage-message t
  "If non-nil, display an usage message for region preview."
  :type 'boolean
  :group 'cycle-region)

(defvar cycle-region-old-region-state nil
  "Holds the state of the region before preview.")

(defun cycle-region-preview ()
  (interactive)
  (setq cycle-region-preview-index 0)
  (when (not cycle-region-ring)
    (user-error "Empty region ring"))
  (setq cycle-region-old-region-state
        (list (region-active-p) (point) (mark)))
  (run-hooks 'cycle-region-pre-preview-hook)
  (let* ((last (ring-ref cycle-region-ring cycle-region-preview-index))
         (point (car last))
         (mark (cdr last)))
    (goto-char point)
    (deactivate-mark)
    (setq cycle-region-preview-overlay (make-overlay point mark))
    (overlay-put cycle-region-preview-overlay 'face 'cycle-region-preview))

  (when cycle-region-display-usage-message
    (message
     (substitute-command-keys
      "\\<cycle-region-preview-map>Use \\[cycle-region-backward] to go back, \\[cycle-region-forward] to go forward, \\[cycle-region-activate] to activate the currently previewed region and q to quit.")))
  (set-transient-map cycle-region-preview-map
                     'cycle-region--preview-keep-p
                     'cycle-region-quit)
  (run-hooks 'cycle-region-post-preview-hook))

(defun cycle-region-backward (arg)
  "Move to the previous region."
  (interactive "p")
  (setq cycle-region-preview-index (+ cycle-region-preview-index arg))
  (let* ((current (ring-ref cycle-region-ring cycle-region-preview-index))
         (point (car current))
         (mark (cdr current)))
    (goto-char point)
    (move-overlay cycle-region-preview-overlay point mark)))

(defun cycle-region-forward (arg)
  "Move to the next region."
  (interactive "p")
  (cycle-region-backward (- arg)))

(defun cycle-region-activate ()
  "Activate the currently previewed region."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (let* ((current (ring-ref cycle-region-ring cycle-region-preview-index))
         (point (car current))
         (mark (cdr current)))
    (goto-char point)
    (set-mark mark)
    (activate-mark)))

(defun cycle-region-quit ()
  "Clean up the region preview overlay."
  (delete-overlay cycle-region-preview-overlay)
  (setq cycle-region-preview-overlay nil)
  (when (and (not (eq this-command 'cycle-region-activate))
             (car cycle-region-old-region-state))
    (goto-char (cadr cycle-region-old-region-state))
    (set-mark (nth 2 cycle-region-old-region-state))
    (activate-mark)))

(provide 'cycle-region)
;;; cycle-region.el ends here
