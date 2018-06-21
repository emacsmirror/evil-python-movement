;;; evil-python-movement-unit-tests.el --- Unit tests for evil-python-movement  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Felipe Lema

;; Author: Felipe Lema <felipel@devuan-pega>
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


;;; Code:
(eval-when-compile
  (require 'cl))
(require 'ert)
(require 'evil)
(require 'f)
(require 's)
;; TODO: (require 'evil-python-movement)

;; Infrastructure
(defvar evil-python-unit-test-python-script-to-test-against
  (concat
   (file-name-as-directory (locate-dominating-file "." "test/test_movement_here.py"))
   "test/test_movement_here.py"))

(assert (file-readable-p evil-python-unit-test-python-script-to-test-against))

(defvar evil-python-unit-test-sample-buffer-cache
  nil
  "Cache for loaded sample Python file.")

(defmacro evil-python-unit-test-with-sample-buffer (&rest body)
  "Load sample buffer and do (with-buffer …BODY…).

Cursor will be placed somewhere around the line annotated as such."
  `(with-temp-buffer
     (insert ,(or evil-python-unit-test-sample-buffer-cache
		  (setq evil-python-unit-test-sample-buffer-cache
			(f-read evil-python-unit-test-python-script-to-test-against))))
     ;; place the cursor at annotated line
     (while (and (not (s-match "#.*CURSOR" (thing-at-point 'line)))
		 (< 1 (save-excursion
			(beginning-of-line)
			(point))))
       (previous-line))
     ;; cursor is ready, execute body
     ,@body
     ))

(defsubst evil-python-unit-test-should-match-comment (expected)
  "Check that current comment should match EXPECTED.

Uses the `should' macro (not `assert')."
  (let* ((current-line (thing-at-point 'line))
	 ;; ⎡# …⎦
	 (full-current-comment (-first-item (s-match "#.*" current-line)))
	 ;; the ⎡…⎦ in ⎡# …⎦ (mind the space)
	 (current-comment (substring full-current-comment 2)))
    (should
     (s-equals? current-comment expected))))

(defsubst evil-python-unit-test-should-be-at-end-of-line ()
  "Check that we're currently looking at the end of line.

Uses the `should' macro (not `assert')."
  (should (= (point)
	     (save-excursion
	       (evil-end-of-line)
	       (point)))))

(ert-deftest evil-python-unit-test-move-to-regex ()
  (with-temp-buffer
    (insert "  aaa\n  bbb  \n  ccc  ")
    (evil-python-move-to-regex "aaa" #'previous-line)
    (should (s-equals? "  aaa\n"
		       (thing-at-point 'line)))
    (evil-python-move-to-regex "ccc" #'next-line)
    (should (s-equals? "  ccc  "
		       (thing-at-point 'line)))))

(ert-deftest evil-python-unit-test-lsb-lsb ()
  "[["
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-lsb-lsb)
   (evil-python-unit-test-should-match-comment "[[ or [m[m")))

(ert-deftest evil-python-unit-test-lsb-lsb×2 ()
  "[[×2"
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-lsb-lsb 2)
   (evil-python-unit-test-should-match-comment "[[[["))
  ;; same test, but inputted differently
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-lsb-lsb)
   (evil-python-move-lsb-lsb)
   (evil-python-unit-test-should-match-comment "[[[[")))

(ert-deftest evil-python-unit-test-lsb-m ()
  "[m"
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-lsb-m)
   (evil-python-unit-test-should-match-comment "[m")))

(ert-deftest evil-python-unit-test-lsb-m×2 ()
  "[m×2"
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-lsb-m 2)
   (evil-python-unit-test-should-match-comment "[[ or [m[m"))
  ;; same test, but inputted differently
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-lsb-m)
   (evil-python-move-lsb-m)
   (evil-python-unit-test-should-match-comment "[[ or [m[m")))

(ert-deftest evil-python-unit-test-rsb-m ()
  "]m"
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-rsb-m)
   (evil-python-unit-test-should-match-comment "]m")))

(ert-deftest evil-python-unit-test-rsb-m×2 ()
  "]m×2"
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-rsb-m 2)
   (evil-python-unit-test-should-match-comment "]] or ]m]m"))
  ;; same test, but inputted differently
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-rsb-m)
   (evil-python-move-rsb-m)
   (evil-python-unit-test-should-match-comment "]] or ]m]m")))

(ert-deftest evil-python-unit-test-lsb-M ()
  "[M"
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-lsb-M)
   (evil-python-unit-test-should-match-comment "[M")
   (evil-python-unit-test-should-be-at-end-of-line)))

(ert-deftest evil-python-unit-test-rsb-M ()
  "]M"
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-rsb-M)
   (evil-python-unit-test-should-match-comment "]M")
   (evil-python-unit-test-should-be-at-end-of-line)))

(ert-deftest evil-python-unit-test-end-of-block ()
  (evil-python-unit-test-with-sample-buffer
   (evil-python-unit-test-should-match-comment "<--- CURSOR")
   (evil-python-move-lsb-lsb)
   (evil-python-py-block-end)
   (evil-python-unit-test-should-match-comment "][")))

(ert-deftest evil-python-unit-test-rsb-lsb ()
  "]["
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-rsb-lsb)
   (evil-python-unit-test-should-match-comment "][")
   (evil-python-unit-test-should-be-at-end-of-line)))

(ert-deftest evil-python-unit-test-rsb-lsb-from-class-def ()
  "][ from class def"
  (evil-python-unit-test-with-sample-buffer
   (evil-python-move-lsb-lsb)
   (should (s-match "^class" (thing-at-point 'line)))
   (evil-python-move-rsb-lsb)
   (evil-python-unit-test-should-match-comment "][")
   (evil-python-unit-test-should-be-at-end-of-line)))

;;(ert-deftest evil-python-unit-test-indentation-and-parentheses ()
;; TODO
;;  )


(provide 'evil-python-movement-unit-tests)
;;; evil-python-movement-unit-tests.el ends here
