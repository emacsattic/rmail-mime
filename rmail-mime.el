;;; rmail-mime.el --- MIME extender for RMAIL

;; Copyright (C) 1985,86,87,88,93,94,95,96,97 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1997/2/17
;; Version: $Id$
;; Keywords: MIME, multimedia, mail

;; This file is part of Emacs.

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


;;; @ for mule and MIME
;;;

(defun rmail-show-mime-message ()
  (rmail-show-all-header)
  (let ((abuf (current-buffer))
	(buf-name (format "*Preview-%s [%d/%d]*"
			  (buffer-name)
			  rmail-current-message rmail-total-messages))
	buf win)
    (if (and mime::article/preview-buffer
	     (setq buf (get-buffer mime::article/preview-buffer))
	     )
	(progn
	  (save-excursion
	    (set-buffer buf)
	    (rename-buffer buf-name)
	    )
	  (if (setq win (get-buffer-window buf))
	      (progn
		(delete-window (get-buffer-window abuf))
		(set-window-buffer win abuf)
		(set-buffer abuf)
		))
	  ))
    (setq win (get-buffer-window abuf))
    (save-window-excursion
      (mime-view-mode nil nil nil nil buf-name
		      'rmail-mime-execute-original-command)
      (or buf
	  (setq buf (current-buffer))
	  )
      )
    (set-window-buffer win buf)
    ))

(set-alist 'mime-text-decoder-alist
	   'rmail-mode
	   (function mime-charset/decode-buffer))

(set-alist 'mime-view-quitting-method-alist
	   'rmail-mode
	   (function rmail-quit)
	   )

(set-alist 'mime-view-over-to-previous-method-alist
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-window-excursion
		(switch-to-buffer mime::preview/article-buffer)
		(rmail-previous-undeleted-message 1)
		))))

(set-alist 'mime-view-over-to-next-method-alist
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-window-excursion
		(switch-to-buffer mime::preview/article-buffer)
		(rmail-next-undeleted-message 1)
		))))

(set-alist 'mime-view-show-summary-method
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-excursion
		(set-buffer mime::preview/article-buffer)
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
	  (switch-to-buffer mime::preview/article-buffer)
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


;;; @ Summary
;;;

;; (defadvice rmail-summary-scroll-msg-up (around rmail-mime activate)
;;   (let ((rmail-buffer
;;          (save-excursion
;;            (set-buffer rmail-buffer)
;;            (if (and (boundp 'mime::article/preview-buffer)
;;                     (get-buffer-window mime::article/preview-buffer)
;;                     )
;;                mime::article/preview-buffer
;;              rmail-buffer)
;;            )))
;;     ad-do-it))

;; (defadvice rmail-summary-next-msg (around rmail-mime activate)
;;   (let ((rmail-buffer (rmail-preview-buffer)))
;;     ad-do-it))

  
;;; @ end
;;;

(provide 'rmail-mime)

(run-hooks 'rmail-mime-load-hook)

;;; rmail-mime.el ends here
