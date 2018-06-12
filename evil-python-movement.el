;;; evil-python-movement.el --- Port Neovim's python movement to Evil  -*- lexical-binding: t; -*-

;; Copyright © 2018  Felipe Lema

;; Author: Felipe Lema <felipelema en mortemale punto org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; First, I was reading about whether Neovim provided movement tools
;;; Python that I didn't know of, when I stumbled with this:
;;; https://stackoverflow.com/a/28284564
;;; After learning that indeed they existed, they weren't present in
;;; Evil.  After opening an issue
;;; https://github.com/emacs-evil/evil/issues/1043
;;; I was explained that these tools wouldn't be available through
;;; Evil.  So, I implemented them myself.

;;; TL;DR: this is best explained looking at https://stackoverflow.com/a/28284564
;;;        or the unit tests (not yet availaible)

;;; Code:

(require 'dash)
(require 'evil)

(defun 😈-🐍-common-python-movement (count noerror new-pos-function mov-name)
  "Try to move to position or report failure.

Try to move to COUNT times to position told by NEW-POS-FUNCTION or report as
MOV-NAME if NOERROR.
Returns new position or nil."
  (catch 'could-not-move
    (when-let ((maybe-new-position
		(-last-item
		 (save-excursion
		   (--map
		    ;; first test if we can move (in this iteration)
		    (if-let ((new-pos (funcall new-pos-function)))
			(goto-char new-pos)
		      ;; else
		      (progn
			(unless noerror
			  (message "Cannot move %s-wise for %n-th time"
				   mov-name it))
			(throw 'could-not-move nil)))
		    (number-sequence 1 count))))))
      (goto-char maybe-new-position))))

;; [[
(evil-define-motion 😈-🐍-move-lsb-lsb (count noerror)
  "Mimic Neovim's [[ movement in Python editing.

See https://github.com/noctuid/evil-guide#command-properties.
Based off `evil-forward-char'."
  :jump t
  :type inclusive
  ;; first, test if movable
  (interactive "<c>" (list (evil-kbd-macro-suppress-motion-error)))
  (😈-🐍-common-python-movement count noerror #'py-backward-top-level "[["))

;; ]]
(evil-define-motion 😈-🐍-move-rsb-rsb (count noerror)
  "Mimic Neovim's ]] movement in Python editing.

See https://github.com/noctuid/evil-guide#command-properties.
Based off `evil-forward-char'."
  :jump t
  :type inclusive
  ;; first, test if movable
  (interactive "<c>" (list (evil-kbd-macro-suppress-motion-error)))
  (😈-🐍-common-python-movement count noerror #'py-forward-top-level "]]")
  )

;; [m
(evil-define-motion 😈-🐍-move-lsb-m (count noerror)
  "Mimic Neovim's [m movement in Python editing.

See https://github.com/noctuid/evil-guide#command-properties.
Based off `evil-forward-char'."
  :jump t
  :type inclusive
  ;; first, test if movable
  (interactive "<c>" (list (evil-kbd-macro-suppress-motion-error)))
  (when-let ((new-pos (save-excursion
			(evil-first-non-blank)
			(let ((p (😈-🐍-common-python-movement
				  count
				  noerror
				  #'py-backward-block-or-clause ;; or py-backward-block?
				  "[m")))
			  p))))
    (goto-char new-pos)))

;; ]m
(evil-define-motion 😈-🐍-move-rsb-m (count noerror)
  "Mimic Neovim's ]m movement in Python editing.

See https://github.com/noctuid/evil-guide#command-properties.
Based off `evil-forward-char'."
  :jump t
  :type inclusive
  ;; first, test if movable
  (interactive "<c>" (list (evil-kbd-macro-suppress-motion-error)))
  (😈-🐍-common-python-movement
   count
   noerror
   #'py-forward-block-or-clause ;; or py-forward-block?
   "[m"))

(defun 😈-🐍-py-block-end (&optional indent)
  "Return the point of end of line of (current) INDENT."
  (interactive
   (list (save-excursion
	   (evil-first-non-blank)
	   (length (buffer-substring-no-properties
		    (line-beginning-position)
		    (point))))))
  (save-excursion
    (while
	(and
	 ;; reached the end of buffer
	 (not (= (line-end-position) (point-max)))
	 (or
	  ;; indentation doesn't change for new def/class
	  (<= indent (save-excursion
		       (evil-first-non-blank)
		       (let ((this-line-indent (buffer-substring-no-properties
						(line-beginning-position)
						(point))))
			 (length this-line-indent))))
	  ;; empty line
	  ))
      (evil-next-line))
    (evil-previous-line)
    (line-end-position)))

;;[M
(evil-define-motion 😈-🐍-move-lsb-M (count noerror)
  "Mimic Neovim's ]M movement in Python editing.

See https://github.com/noctuid/evil-guide#command-properties.
Based off `evil-forward-char'."
  :jump t
  :type inclusive ;; (-any '(line inclusive exclusive block) )
  (interactive "<c>" (list (evil-kbd-macro-suppress-motion-error)))
  (if-let ((new-pos (save-excursion
		      (evil-first-non-blank)
		      (😈-🐍-common-python-movement
		       count
		       noerror
		       #'py-backward-block-or-clause ;; or py-backward-block?
		       "[m"))))
      (progn
	(goto-char new-pos)
	(goto-char (call-interactively '😈-🐍-py-block-end))
	(evil-end-of-line))))

;;]M
(evil-define-motion 😈-🐍-move-lsb-M (count noerror)
  (error "Not implemented."))






  ;; http://ergoemacs.org/emacs/elisp_run_elisp_when_file_opens.html
  ;; Local Variables:
  ;; eval: (evil-set-register ?e "😈-🐍-"(evil-set-register ?e "😈-🐍-"))
  ;; End:
  (provide 'evil-python-movement)
;;; evil-python-movement.el ends here
