;;; rmail-mime.el --- MIME extender for RMAIL

;; Copyright (C) 1985,86,87,88,93,94,95,96,97,98 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1997/2/17
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
(require 'alist)

(eval-when-compile
  (load "rmailsum")
  )

(setq rmail-show-mime-function (function rmail-show-mime-message))
(setq rmail-summary-line-decoder
      (function
       (lambda (string)
	 (eword-decode-string
	  (decode-coding-string string 'undecided)
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
    (if (and mime-preview-buffer
	     (setq buf (get-buffer mime-preview-buffer))
	     )
	(if (setq win (get-buffer-window buf))
	    (let ((w (get-buffer-window abuf)))
	      (if w (delete-window w))
	      (set-window-buffer win abuf)
	      (set-buffer abuf)
	      ))
      )
    (setq win (get-buffer-window abuf))
    (save-window-excursion
      (mime-view-buffer nil buf-name nil
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
    (or (get-buffer-window buf t) (set-window-buffer win buf))
    (make-local-variable 'rmail-buffer)
    (setq rmail-buffer abuf)
    (let ((mode-line
	   (save-excursion
	     (set-buffer abuf)
	     (setq rmail-view-buffer buf)
	     mode-line-process)))
      (setq mode-line-process mode-line)
      )))

(set-alist 'mime-raw-representation-type-alist 'rmail-mode 'binary)

(defun rmail-mime-quit ()
  (interactive)
  (if (eq major-mode 'mime-view-mode)
      (progn
	(switch-to-buffer mime-raw-buffer)
	(bury-buffer mime-preview-buffer)
	))
  (let (rmail-enable-mime)
    (rmail-quit)
    ))

(set-alist 'mime-preview-quitting-method-alist
	   'rmail-mode #'rmail-mime-quit)

(set-alist 'mime-preview-over-to-previous-method-alist
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-window-excursion
		(switch-to-buffer mime-raw-buffer)
		(rmail-previous-undeleted-message 1)
		))))

(set-alist 'mime-preview-over-to-next-method-alist
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-window-excursion
		(switch-to-buffer mime-raw-buffer)
		(rmail-next-undeleted-message 1)
		))))


(cond
 ((featurep 'xemacs)
  ;; for XEmacs

(defun rmail-mime-execute-original-command ()
  (interactive)
  (let* ((seq (vector last-command-event))
	 (ret (lookup-key rmail-mode-map seq))
	 )
    (if (and ret
	     (progn
	       (while (keymapp (setq ret (lookup-key rmail-mode-map seq)))
		 (setq seq (vconcat seq (vector (next-event))))
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
      (setq ret (or (lookup-key global-map seq)
		    (and (setq ret (lookup-key function-key-map seq))
			 (lookup-key global-map ret))))
      (while (keymapp ret)
	(setq ret (lookup-key ret (vector (next-event))))
	)
      (and (commandp ret)
	   (call-interactively ret))
      )))
)
(t
 ;; for Emacs

(defun rmail-mime-execute-original-command ()
  (interactive)
  (let* ((seq (if (and (symbolp last-command-event)
		       (eq (get last-command-event 'ascii-character)
			   meta-prefix-char))
		  (vector meta-prefix-char (read-event))
		(vector last-command-event)
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
	  (and (window-live-p pwin)
	       (not (eq ret 'rmail-input))
	       (set-window-buffer pwin pbuf)
	    ))
      (if (and (eq (event-basic-type (aref seq 0)) (aref mouse-button-2 0))
	       (eq (event-point (aref seq 0)) 'vertical-scroll-bar))
	  (scroll-bar-drag (aref seq 0))
	(setq ret (or (lookup-key global-map seq)
		      (and (setq ret (lookup-key function-key-map seq))
			   (lookup-key global-map ret))))
	(while (keymapp ret)
	  (setq ret (lookup-key ret (vector (read-event))))
	  )
	(and (commandp ret)
	     (call-interactively ret)
	     )
	)
      )))
))


;;; @ end
;;;

(provide 'rmail-mime)

;;; rmail-mime.el ends here
