;;; rmail-mime.el --- MIME extender for RMAIL

;; Copyright (C) 1985,86,87,88,93,94,95,96,97 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1997/2/17
;; Version: $Id$
;; Keywords: MIME, multimedia, mail

;; This file is not part of Emacs yet.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'rmail)
(require 'mime-view)

(eval-when-compile
  (load "rmailsum")
  )

(setq rmail-show-mime-function (function rmail-show-mime-message))
(setq rmail-summary-line-decoder
      (function
       (lambda (string)
	 (eword-decode-string
	  (decode-coding-string string 'automatic-conversion)
	  ))))


;;; @ for mule and MIME
;;;

(defun rmail-show-all-header ()
  (rmail-maybe-set-message-counters)
  (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (forward-line 1)
    (if (= (following-char) ?1)
	(progn
	  (delete-char 1)
	  (insert ?0)
	  (forward-line 1)
	  (let ((case-fold-search t))
	    (while (looking-at "Summary-Line:\\|Mail-From:")
	      (forward-line 1)))
	  (insert "*** EOOH ***\n")
	  (forward-char -1)
	  (search-forward "\n*** EOOH ***\n")
	  (forward-line -1)
	  (let ((temp (point)))
	    (and (search-forward "\n\n" nil t)
		 (delete-region temp (point))))
	  (goto-char (point-min))
	  (search-forward "\n*** EOOH ***\n")
	  (narrow-to-region (point) (point-max)))
      )))

(defun rmail-show-mime-message ()
  (rmail-show-all-header)
  (let ((abuf (current-buffer))
	(buf-name (concat "*View-" (buffer-name) "*"))
	buf win)
    (if (and mime-view-buffer
	     (setq buf (get-buffer mime-view-buffer))
	     )
	(if (setq win (get-buffer-window buf))
	    (progn
	      (delete-window (get-buffer-window abuf))
	      (set-window-buffer win abuf)
	      (set-buffer abuf)
	      ))
      )
    (setq win (get-buffer-window abuf))
    (save-window-excursion
      (mime-view-mode nil nil nil nil buf-name
		      'rmail-mime-execute-original-command)
      (or buf
	  (setq buf (current-buffer))
	  )
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults
	    '(rmail-font-lock-keywords
	      t nil nil nil
	      (font-lock-maximum-size . nil)
	      (font-lock-fontify-buffer-function
	       . rmail-fontify-buffer-function)
	      (font-lock-unfontify-buffer-function
	       . rmail-unfontify-buffer-function)
	      (font-lock-inhibit-thing-lock
	       . (lazy-lock-mode fast-lock-mode))))
      (run-hooks 'rmail-show-mime-message-hook)
      )
    (set-window-buffer win buf)
    (make-local-variable 'rmail-buffer)
    (setq rmail-buffer abuf)
    (let ((mode-line
	   (save-excursion
	     (set-buffer abuf)
	     (setq rmail-view-buffer buf)
	     mode-line-process)))
      (setq mode-line-process mode-line)
      )))

(set-alist 'mime-text-decoder-alist
	   'rmail-mode
	   (function mime-text-decode-buffer))

(defun rmail-mime-quit ()
  (interactive)
  (if (eq major-mode 'mime-view-mode)
      (progn
	(switch-to-buffer mime-raw-buffer)
	(bury-buffer mime-view-buffer)
	))
  (let (rmail-enable-mime)
    (rmail-quit)
    ))

(set-alist 'mime-view-quitting-method-alist
	   'rmail-mode
	   (function rmail-mime-quit))

(set-alist 'mime-view-over-to-previous-method-alist
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-window-excursion
		(switch-to-buffer mime-raw-buffer)
		(rmail-previous-undeleted-message 1)
		))))

(set-alist 'mime-view-over-to-next-method-alist
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-window-excursion
		(switch-to-buffer mime-raw-buffer)
		(rmail-next-undeleted-message 1)
		))))

(set-alist 'mime-view-show-summary-method
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-excursion
		(set-buffer mime-raw-buffer)
		(rmail-summary)
		))))

(defun rmail-mime-execute-original-command ()
  (interactive)
  (let* ((seq (if (and (symbolp last-input-event)
		       (eq (get last-input-event 'ascii-character)
			   meta-prefix-char)
		       )
		  (vector meta-prefix-char (read-event))
		(vector last-input-event)
		))
	 (ret (lookup-key rmail-mode-map seq))
	 )
    (if (and ret
	     (progn
	       (while (keymapp (setq ret (lookup-key rmail-mode-map seq)))
		 (setq seq (vconcat seq (vector (read-event))))
		 )
	       (commandp ret)
	       ))
	(let ((pwin (selected-window))
	      (pbuf (current-buffer))
	      )
	  (switch-to-buffer mime-raw-buffer)
	  (call-interactively ret)
	  (if (window-live-p pwin)
	      (set-window-buffer pwin pbuf)
	    ))
      (setq ret (lookup-key global-map seq))
      (while (keymapp ret)
	(setq ret (lookup-key ret (vector (read-event))))
	)
      (call-interactively ret)
      )))

  
;;; @ end
;;;

(provide 'rmail-mime)

;;; rmail-mime.el ends here
