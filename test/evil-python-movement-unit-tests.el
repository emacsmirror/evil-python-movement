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
(defvar 😈-🐍-unit-test-python-script-to-test-against
  (concat
   (file-name-as-directory (locate-dominating-file "." "test/test_movement_here.py"))
   "test/test_movement_here.py"))

(assert (file-readable-p 😈-🐍-unit-test-python-script-to-test-against))

(defvar 😈-🐍-unit-test-sample-buffer-cache
  nil
  "Cache for loaded sample Python file.")

(defmacro 😈-🐍-unit-test-with-sample-buffer (&rest body)
  "Load sample buffer and do (with-buffer …BODY…).

Cursor will be placed somewhere around the line annotated as such."
  `(with-temp-buffer
     (insert ,(or 😈-🐍-unit-test-sample-buffer-cache
		  (setq 😈-🐍-unit-test-sample-buffer-cache
			(f-read 😈-🐍-unit-test-python-script-to-test-against))))
     ;; place the cursor at annotated line
     (while (and (not (s-match "#.*CURSOR" (thing-at-point 'line)))
		 (< 1 (save-excursion
			(beginning-of-line)
			(point))))
       (previous-line))
     ;; cursor is ready, execute body
     ,@body
     ))

(defsubst 😈-🐍-unit-test-should-match-comment (expected)
  "Check that current comment should match EXPECTED.

Uses the `should' macro (not `assert')."
  (let* ((current-line (thing-at-point 'line))
	 ;; ⎡# …⎦
	 (full-current-comment (-first-item (s-match "#.*" current-line)))
	 ;; the ⎡…⎦ in ⎡# …⎦ (mind the space)
	 (current-comment (substring full-current-comment 2)))
    (should
     (s-equals? current-comment expected))))

(ert-deftest 😈-🐍-unit-test-move-to-regex ()
  (with-temp-buffer
    (insert "  aaa\n  bbb  \n  ccc  ")
    (😈-🐍-move-to-regex "aaa" #'previous-line)
    (should (s-equals? "  aaa\n"
		       (thing-at-point 'line)))
    (😈-🐍-move-to-regex "ccc" #'next-line)
    (should (s-equals? "  ccc  "
		       (thing-at-point 'line)))))

(ert-deftest 😈-🐍-unit-test-lsb-lsb ()
  "[["
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-lsb-lsb)
   (😈-🐍-unit-test-should-match-comment "[[ or [m[m")))

(ert-deftest 😈-🐍-unit-test-lsb-lsb×2 ()
  "[[×2"
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-lsb-lsb 2)
   (😈-🐍-unit-test-should-match-comment "[[[["))
  ;; same test, but inputted differently
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-lsb-lsb)
   (😈-🐍-move-lsb-lsb)
   (😈-🐍-unit-test-should-match-comment "[[[[")))

(ert-deftest 😈-🐍-unit-test-lsb-m ()
  "[m"
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-lsb-m)
   (😈-🐍-unit-test-should-match-comment "[m")))

(ert-deftest 😈-🐍-unit-test-lsb-m×2 ()
  "[m×2"
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-lsb-m 2)
   (😈-🐍-unit-test-should-match-comment "[[ or [m[m"))
  ;; same test, but inputted differently
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-lsb-m)
   (😈-🐍-move-lsb-m)
   (😈-🐍-unit-test-should-match-comment "[[ or [m[m")))

(ert-deftest 😈-🐍-unit-test-rsb-m ()
  "]m"
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-rsb-m)
   (😈-🐍-unit-test-should-match-comment "]m")))

(ert-deftest 😈-🐍-unit-test-rsb-m×2 ()
  "]m×2"
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-rsb-m 2)
   (😈-🐍-unit-test-should-match-comment "]] or ]m]m"))
  ;; same test, but inputted differently
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-rsb-m)
   (😈-🐍-move-rsb-m)
   (😈-🐍-unit-test-should-match-comment "]] or ]m]m")))

(ert-deftest 😈-🐍-unit-test-lsb-M ()
  "[M"
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-lsb-M)
   (😈-🐍-unit-test-should-match-comment "[M")
   ;; test at end of line
   (should (= (point)
	      (save-excursion
		(evil-end-of-line)
		(point))))))

(ert-deftest 😈-🐍-unit-test-rsb-M ()
  "]M"
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-move-rsb-M)
   (😈-🐍-unit-test-should-match-comment "]M")
   ;; test at end of line
   (should (= (point)
	      (save-excursion
		(evil-end-of-line)
		(point))))))

(ert-deftest 😈-🐍-unit-test-end-of-block ()
  (😈-🐍-unit-test-with-sample-buffer
   (😈-🐍-unit-test-should-match-comment "<--- CURSOR")
   (😈-🐍-move-lsb-lsb)
   (😈-🐍-py-block-end)
   (😈-🐍-unit-test-should-match-comment "][")))
;;(ert-deftest 😈-🐍-unit-test-indentation-and-parentheses ()
;; TODO
;;  )


;; http://ergoemacs.org/emacs/elisp_run_elisp_when_file_opens.html
;; Local Variables:
;; eval: (evil-set-register ?u "😈-🐍-unit-test-")
;; End:
(provide 'evil-python-movement-unit-tests)
;;; evil-python-movement-unit-tests.el ends here
