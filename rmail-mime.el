;;; rmail-mime.el --- MIME extender for RMAIL

;; Copyright (C) 1985,86,87,88,93,94,95,96,97,98,99 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
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

;; (defun rmail-show-all-header ()
;;   (rmail-maybe-set-message-counters)
;;   (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
;;   (let ((buffer-read-only nil))
;;     (goto-char (point-min))
;;     (forward-line 1)
;;     (if (= (following-char) ?1)
;;         (progn
;;           (delete-char 1)
;;           (insert ?0)
;;           (forward-line 1)
;;           (let ((case-fold-search t))
;;             (while (looking-at "Summary-Line:\\|Mail-From:")
;;               (forward-line 1)))
;;           (insert "*** EOOH ***\n")
;;           (forward-char -1)
;;           (search-forward "\n*** EOOH ***\n")
;;           (forward-line -1)
;;           (let ((temp (point)))
;;             (and (search-forward "\n\n" nil t)
;;                  (delete-region temp (point))))
;;           (goto-char (point-min))
;;           (search-forward "\n*** EOOH ***\n")
;;           (narrow-to-region (point) (point-max)))
;;       )))

(defun rmail-show-mime-message ()
  ;; (rmail-show-all-header)
  (rmail-toggle-header 0)
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
      (make-local-variable 'rmail-buffer)
      (setq rmail-buffer abuf)
      (let ((mode-line
	     (save-excursion
	       (set-buffer abuf)
	       (setq rmail-view-buffer buf)
	       mode-line-process)))
	(setq mode-line-process mode-line)
	)
      )
    (or (get-buffer-window buf t) (set-window-buffer win buf))
    ))

(set-alist 'mime-raw-representation-type-alist 'rmail-mode 'binary)

(defun rmail-mime-quit ()
  (interactive)
  (if (eq major-mode 'mime-view-mode)
      (let ((entity (get-text-property (point-min) 'mime-view-entity))
	    (buf (current-buffer)))
	(when entity
	  (switch-to-buffer (mime-entity-header-buffer entity))
          ;; (switch-to-buffer mime-raw-buffer)
          ;; (bury-buffer mime-preview-buffer)
	  (bury-buffer buf))))
  (let (rmail-enable-mime)
    (rmail-quit)
    ))

(set-alist 'mime-preview-quitting-method-alist
	   'rmail-mode #'rmail-mime-quit)

(set-alist 'mime-preview-over-to-previous-method-alist
	   'rmail-mode
	   (function
	    (lambda ()
	      (let ((entity (get-text-property (point-min) 'mime-view-entity)))
		(save-window-excursion
		  (when entity
		    (switch-to-buffer (mime-entity-header-buffer entity))
		    (rmail-previous-undeleted-message 1)
		    ))))))

(set-alist 'mime-preview-over-to-next-method-alist
	   'rmail-mode
	   (function
	    (lambda ()
	      (save-window-excursion
		(let ((entity (get-text-property (point-min)
						 'mime-view-entity)))
		  (when entity
		    (switch-to-buffer (mime-entity-header-buffer entity))
		    (rmail-next-undeleted-message 1)
		    ))))))


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
	      (entity (get-text-property (point-min) 'mime-view-entity)))
	  (if entity
	      (switch-to-buffer (mime-entity-header-buffer entity)))
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
	      (entity (get-text-property (point-min) 'mime-view-entity)))
	  (if entity
	      (switch-to-buffer (mime-entity-header-buffer entity)))
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

(eval-after-load "rmailsum"
  '(defun rmail-new-summary (description redo-form function &rest args)
     "Create a summary of selected messages.
DESCRIPTION makes part of the mode line of the summary buffer.
For each message, FUNCTION is applied to the message number and ARGS...
and if the result is non-nil, that message is included.
nil for FUNCTION means all messages."
  (message "Computing summary lines...")
  (let (sumbuf mesg was-in-summary)
    (save-excursion
      ;; Go to the Rmail buffer.
      (cond ((eq major-mode 'rmail-summary-mode)
	     (setq was-in-summary t)
	     (set-buffer rmail-buffer))
	    (rmail-buffer
	     (set-buffer rmail-buffer)))
      ;; Find its summary buffer, or make one.
      (setq sumbuf
	    (if (and rmail-summary-buffer
		     (buffer-name rmail-summary-buffer))
		rmail-summary-buffer
	      (generate-new-buffer (concat (buffer-name) "-summary"))))
      (setq mesg rmail-current-message)
      ;; Filter the messages; make or get their summary lines.
      (let ((summary-msgs ())
	    (new-summary-line-count 0))
	(let ((msgnum 1)
	      (buffer-read-only nil)
	      (old-min (point-min-marker))
	      (old-max (point-max-marker)))
	  ;; Can't use save-restriction here; that doesn't work if we
	  ;; plan to modify text outside the original restriction.
	  (save-excursion
	    (widen)
	    (goto-char (point-min))
	    (while (>= rmail-total-messages msgnum)
	      (if (or (null function)
		      (apply function (cons msgnum args)))
		  (setq summary-msgs
			(cons (cons msgnum (rmail-make-summary-line msgnum))
			      summary-msgs)))
	      (setq msgnum (1+ msgnum)))
	    (setq summary-msgs (nreverse summary-msgs)))
	  (narrow-to-region old-min old-max))
	;; Temporarily, while summary buffer is unfinished,
	;; we "don't have" a summary.
	(setq rmail-summary-buffer nil)
	(save-excursion
	  (let ((rbuf (current-buffer))
		(vbuf rmail-view-buffer)
		(total rmail-total-messages))
	    (set-buffer sumbuf)
	    ;; Set up the summary buffer's contents.
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (while summary-msgs
		(princ (cdr (car summary-msgs)) sumbuf)
		(setq summary-msgs (cdr summary-msgs)))
	      (goto-char (point-min)))
	    ;; Set up the rest of its state and local variables.
	    (setq buffer-read-only t)
	    (rmail-summary-mode)
	    (make-local-variable 'minor-mode-alist)
	    (setq minor-mode-alist (list (list t (concat ": " description))))
	    (setq rmail-buffer rbuf
		  rmail-view-buffer vbuf
		  rmail-summary-redo redo-form
		  rmail-total-messages total))))
      (setq rmail-summary-buffer sumbuf))
    ;; Now display the summary buffer and go to the right place in it.
    (or was-in-summary
	(progn
	  (if (and (one-window-p)
		   pop-up-windows (not pop-up-frames))
	      ;; If there is just one window, put the summary on the top.
	      (progn
		(split-window (selected-window) rmail-summary-window-size)
		(select-window (next-window (frame-first-window)))
		(pop-to-buffer sumbuf)
		;; If pop-to-buffer did not use that window, delete that
		;; window.  (This can happen if it uses another frame.)
		(if (not (eq sumbuf (window-buffer (frame-first-window))))
		    (delete-other-windows)))
	    (pop-to-buffer sumbuf))
	  (set-buffer rmail-buffer)
	  ;; This is how rmail makes the summary buffer reappear.
	  ;; We do this here to make the window the proper size.
	  (rmail-select-summary nil)
	  (set-buffer rmail-summary-buffer)))
    (rmail-summary-goto-msg mesg t t)
    (rmail-summary-construct-io-menu)
    (message "Computing summary lines...done")))
  )


;;; @ end
;;;

(provide 'rmail-mime)

;;; rmail-mime.el ends here
