;;; rmail-mime.el --- MIME extender for RMAIL

;; Copyright (C) 1985,86,87,88,93,94,95,96 Free Software Foundation, Inc.
;; Copyright (C) 1996,1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1997/2/17
;; Version: $Revision$
;; Keywords: MIME, multimedia, mail

;; This file is part of RMAIL-MIME.

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


;;; @ version
;;;

(defconst rmail-mime-RCS-ID
  "$Id$")

(defconst rmail-mime-version (get-version-string rmail-mime-RCS-ID))


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


;;; @ Rmail Mode
;;;

;;;###autoload
(defun rmail (&optional file-name-arg)
  "Read and edit incoming mail.
Moves messages into file named by `rmail-file-name' (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with file name as argument; then performs rmail editing on
that file, but does not copy any new mail into the file.
Interactively, if you supply a prefix argument, then you
have a chance to specify a file name with the minibuffer.

If `rmail-display-summary' is non-nil, make a summary for this RMAIL file."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Run rmail on RMAIL file: "))))
  (let* ((file-name (expand-file-name (or file-name-arg rmail-file-name)))
	 (existed (get-file-buffer file-name))
	 run-mail-hook)
    ;; Like find-file, but in the case where a buffer existed
    ;; and the file was reverted, recompute the message-data.
    (as-binary-input-file
     (if (and existed (not (verify-visited-file-modtime existed)))
	 (progn
	   ;; Don't be confused by apparent local-variables spec
	   ;; in the last message in the RMAIL file.
	   (let ((enable-local-variables nil))
	     (find-file file-name))
	   (if (and (verify-visited-file-modtime existed)
		    (eq major-mode 'rmail-mode))
	       (progn (rmail-forget-messages)
		      (rmail-set-message-counters))))
       (let ((enable-local-variables nil))
	 (find-file file-name)))
     )
    (if (eq major-mode 'rmail-edit-mode)
	(error "Exit Rmail Edit mode before getting new mail."))
    (if (and existed (> (buffer-size) 0))
	;; Buffer not new and not empty; ensure in proper mode, but that's all.
	(or (eq major-mode 'rmail-mode)
	    (progn (rmail-mode-2)
		   (setq run-mail-hook t)))
      (setq run-mail-hook t)
      (rmail-mode-2)
      ;; Convert all or part to Babyl file if possible.
      (rmail-convert-file)
      (goto-char (point-max))
      (if (null rmail-inbox-list)
	  (progn
	    (rmail-set-message-counters)
	    (rmail-show-message))))
    (or (and (null file-name-arg)
	     (rmail-get-new-mail))
	(rmail-show-message (rmail-first-unseen-message)))
    (if rmail-display-summary (rmail-summary))
    (rmail-construct-io-menu)
    (if run-mail-hook
	(run-hooks 'rmail-mode-hook))))

(defun rmail-quit ()
  "Quit out of RMAIL."
  (interactive)
  (if (eq major-mode 'mime-view-mode)
      (let ((buf mime::preview/article-buffer)
	    (pbuf (current-buffer))
	    )
	(switch-to-buffer buf)
	(bury-buffer pbuf)
	))
  (let (rmail-show-mime)
    (rmail-expunge-and-save)
    )
  ;; Don't switch to the summary buffer even if it was recently visible.
  (if rmail-summary-buffer
      (progn
	(replace-buffer-in-windows rmail-summary-buffer)
	(bury-buffer rmail-summary-buffer)))
  (let ((obuf (current-buffer)))
    (replace-buffer-in-windows obuf)
    (bury-buffer obuf)))


;;; @ Rmail input
;;;

(defun rmail-insert-inbox-text (files renamep)
  ;; Detect a locked file now, so that we avoid moving mail
  ;; out of the real inbox file.  (That could scare people.)
  (or (memq (file-locked-p buffer-file-name) '(nil t))
      (error "RMAIL file %s is locked"
	     (file-name-nondirectory buffer-file-name)))
  (let (file tofile delete-files movemail popmail)
    (while files
      (setq file (file-truename
		  (expand-file-name (substitute-in-file-name (car files))))
	    tofile (expand-file-name
		    ;; Generate name to move to from inbox name,
		    ;; in case of multiple inboxes that need moving.
		    (concat ".newmail-" (file-name-nondirectory file))
		    ;; Use the directory of this rmail file
		    ;; because it's a nuisance to use the homedir
		    ;; if that is on a full disk and this rmail
		    ;; file isn't.
		    (file-name-directory
		     (expand-file-name buffer-file-name))))
      ;; Always use movemail to rename the file,
      ;; since there can be mailboxes in various directories.
      (setq movemail t)
;;;      ;; If getting from mail spool directory,
;;;      ;; use movemail to move rather than just renaming,
;;;      ;; so as to interlock with the mailer.
;;;      (setq movemail (string= file
;;;			      (file-truename
;;;			       (concat rmail-spool-directory
;;;				       (file-name-nondirectory file)))))
      (setq popmail (string-match "^po:" (file-name-nondirectory file)))
      (if popmail (setq file (file-name-nondirectory file)
			renamep t))
      (if movemail
	  (progn
	    ;; On some systems, /usr/spool/mail/foo is a directory
	    ;; and the actual inbox is /usr/spool/mail/foo/foo.
	    (if (file-directory-p file)
		(setq file (expand-file-name (user-login-name)
					     file)))))
      (cond (popmail
	     (if (and rmail-pop-password-required (not rmail-pop-password))
		 (setq rmail-pop-password
		       (rmail-read-passwd
			(format "Password for %s: "
				(substring file (+ popmail 3))))))
	     (if (eq system-type 'windows-nt)
		 ;; cannot have "po:" in file name
		 (setq tofile
		       (expand-file-name
			(concat ".newmail-pop-" (substring file (+ popmail 3)))
			(file-name-directory
			 (expand-file-name buffer-file-name)))))
	     (message "Getting mail from post office ..."))
	    ((and (file-exists-p tofile)
		  (/= 0 (nth 7 (file-attributes tofile))))
	     (message "Getting mail from %s..." tofile))
	    ((and (file-exists-p file)
		  (/= 0 (nth 7 (file-attributes file))))
	     (message "Getting mail from %s..." file)))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
	     (setq tofile file))
	    ((or (file-exists-p tofile) (and (not popmail)
					     (not (file-exists-p file))))
	     nil)
	    ((and (not movemail) (not popmail))
	     ;; Try copying.  If that fails (perhaps no space),
	     ;; rename instead.
	     (condition-case nil
		 (copy-file file tofile nil)
	       (error
		;; Third arg is t so we can replace existing file TOFILE.
		(rename-file file tofile t)))
	     ;; Make the real inbox file empty.
	     ;; Leaving it deleted could cause lossage
	     ;; because mailers often won't create the file.
	     (condition-case ()
		 (write-region (point) (point) file)
	       (file-error nil)))
	    (t
	     (let ((errors nil))
	       (unwind-protect
		   (save-excursion
		     (setq errors (generate-new-buffer " *rmail loss*"))
		     (buffer-disable-undo errors)
		     (if rmail-pop-password
			 (call-process
			  (or rmail-movemail-program
			      (expand-file-name "movemail" exec-directory))
			  nil errors nil file tofile rmail-pop-password)
		       (call-process
			(or rmail-movemail-program
			    (expand-file-name "movemail" exec-directory))
			nil errors nil file tofile))
		     (if (not (buffer-modified-p errors))
			 ;; No output => movemail won
			 nil
		       (set-buffer errors)
		       (subst-char-in-region (point-min) (point-max)
					     ?\n ?\  )
		       (goto-char (point-max))
		       (skip-chars-backward " \t")
		       (delete-region (point) (point-max))
		       (goto-char (point-min))
		       (if (looking-at "movemail: ")
			   (delete-region (point-min) (match-end 0)))
		       (beep t)
		       (message "movemail: %s"
				(buffer-substring (point-min)
						  (point-max)))
		       (sit-for 3)
		       nil))
		 (if errors (kill-buffer errors))))))
      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (let (size)
	    (goto-char (point-max))
	    ;; At first, read the file as is (i.e. no decoding).
	    (setq size (nth 1 (as-binary-input-file
			       (insert-file-contents tofile))))
	    ;;; 1996/12/9 by MORIOKA Tomohiko <morioka@jaist.ac.jp>
	    ;;;	Don't code-convert for RMAIL file
	    ;; Then, decode the contents one by one.
            ;; (let ((pos (point)))
            ;;   (cond ((looking-at "^From ") ; new mails.
            ;;          (forward-line 1)
            ;;          (while (not (eobp))
            ;;            (setq pos (point))
            ;;            (search-forward "\nFrom " nil 'move)
            ;;            (decode-coding-region pos (point)
            ;;                                  'automatic-conversion)))
            ;;         ((looking-at "BABYL OPTIONS:\\|\^L") ; Babyl format
            ;;          (while (not (eobp))
            ;;            (setq pos (point))
            ;;            (search-forward "\n^_" nil 'move)
            ;;            (decode-coding-region pos (point)
            ;;                                  rmail-file-coding-system)))
            ;;         (t ; Perhaps MMDF format.  Convert all data at once.
            ;;          (decode-coding-region (point) (point-max)
            ;;                                'automatic-conversion))))
	    (goto-char (point-max))
	    (or (= (preceding-char) ?\n)
		(zerop size)
		(insert ?\n))
	    (setq delete-files (cons tofile delete-files))))
      (message "")
      (setq files (cdr files)))
    delete-files))


;; the  rmail-break-forwarded-messages  feature is not implemented
(defun rmail-convert-to-babyl-format ()
  (let ((count 0) start
	(case-fold-search nil)
	(invalid-input-resync
	 (function (lambda ()
		     (message "Invalid Babyl format in inbox!")
		     (sit-for 3)
		     ;; Try to get back in sync with a real message.
		     (if (re-search-forward
			  (concat mmdf-delim1 "\\|^From") nil t)
			 (beginning-of-line)
		       (goto-char (point-max)))))))
    (goto-char (point-min))
    (save-restriction
      (while (not (eobp))
	(cond ((looking-at "BABYL OPTIONS:");Babyl header
	       (if (search-forward "\n\^_" nil t)
		   ;; If we find the proper terminator, delete through there.
		   (delete-region (point-min) (point))
		 (funcall invalid-input-resync)
		 (delete-region (point-min) (point))))
	      ;; Babyl format message
	      ((looking-at "\^L")
	       (or (search-forward "\n\^_" nil t)
		   (funcall invalid-input-resync))
	       (setq count (1+ count))
	       ;; Make sure there is no extra white space after the ^_
	       ;; at the end of the message.
	       ;; Narrowing will make sure that whatever follows the junk
	       ;; will be treated properly.
	       (delete-region (point)
			      (save-excursion
				(skip-chars-forward " \t\n")
				(point)))
	       (narrow-to-region (point) (point-max)))
	      ;;*** MMDF format
	      ((let ((case-fold-search t))
		 (looking-at mmdf-delim1))
	       (let ((case-fold-search t))
		 (replace-match "\^L\n0, unseen,,\n*** EOOH ***\n")
		 (setq start (point))
		 (re-search-forward mmdf-delim2 nil t)
		 (replace-match "\^_"))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (1- (point)))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char "\^_"
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (narrow-to-region (point) (point-max))
	       (setq count (1+ count)))
	      ;;*** Mail format
	      ((looking-at "^From ")
	       (setq start (point))
	       (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	       (rmail-nuke-pinhead-header)
	       ;; If this message has a Content-Length field,
	       ;; skip to the end of the contents.
	       (let* ((header-end (save-excursion
				    (and (re-search-forward "\n\n" nil t)
					 (1- (point)))))
		      (case-fold-search t)
		      (size
		       ;; Get the numeric value from the Content-Length field.
		       (save-excursion
			 ;; Back up to end of prev line,
			 ;; in case the Content-Length field comes first.
			 (forward-char -1)
			 (and (search-forward "\ncontent-length: "
					      header-end t)
			      (let ((beg (point))
				    (eol (progn (end-of-line) (point))))
				(string-to-int (buffer-substring beg eol)))))))
		 (and size
		      (if (and (natnump size)
			       (<= (+ header-end size) (point-max))
			       ;; Make sure this would put us at a position
			       ;; that we could continue from.
			       (save-excursion
				 (goto-char (+ header-end size))
				 (skip-chars-forward "\n")
				 (or (eobp)
				     (and (looking-at "BABYL OPTIONS:")
					  (search-forward "\n\^_" nil t))
				     (and (looking-at "\^L")
					  (search-forward "\n\^_" nil t))
				     (let ((case-fold-search t))
				       (looking-at mmdf-delim1))
				     (looking-at "From "))))
			  (goto-char (+ header-end size))
			(message "Ignoring invalid Content-Length field")
			(sit-for 1 0 t))))

	       (if (re-search-forward
		    (concat "^[\^_]?\\("
			    rmail-unix-mail-delimiter
			    "\\|"
			    mmdf-delim1 "\\|"
			    "^BABYL OPTIONS:\\|"
			    "\^L\n[01],\\)") nil t)
		   (goto-char (match-beginning 1))
		 (goto-char (point-max)))
	       (setq count (1+ count))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (insert ?\^_)
	       (narrow-to-region (point) (point-max)))
	      ;;
	      ;; This kludge is because some versions of sendmail.el
	      ;; insert an extra newline at the beginning that shouldn't
	      ;; be there.  sendmail.el has been fixed, but old versions
	      ;; may still be in use.  -- rms, 7 May 1993.
	      ((eolp) (delete-char 1))
	      (t (error "Cannot convert to babyl format")))))
    count))


;;; @ Rmail display Message
;;;

(defun rmail-show-message (&optional n no-summary)
  "Show message number N (prefix argument), counting from start of file.
If summary buffer is currently displayed, update current message there also."
  (interactive "p")
  (if (eq major-mode 'mime-view-mode)
      (switch-to-buffer mime::preview/article-buffer)
    )
  (rmail-maybe-set-message-counters)
  (widen)
  (if (zerop rmail-total-messages)
      (progn (narrow-to-region (point-min) (1- (point-max)))
	     (goto-char (point-min))
	     (setq mode-line-process nil))
    (let (blurb)
      (if (not n)
	  (setq n rmail-current-message)
	(cond ((<= n 0)
	       (setq n 1
		     rmail-current-message 1
		     blurb "No previous message"))
	      ((> n rmail-total-messages)
	       (setq n rmail-total-messages
		     rmail-current-message rmail-total-messages
		     blurb "No following message"))
	      (t
	       (setq rmail-current-message n))))
      (let ((beg (rmail-msgbeg n)))
	(goto-char beg)
	(forward-line 1)
	;; Clear the "unseen" attribute when we show a message.
	(rmail-set-attribute "unseen" nil)
	;; Reformat the header, or else find the reformatted header.
	(let ((end (rmail-msgend n)))
	  (if (= (following-char) ?0)
	      (rmail-reformat-message beg end)
	    (search-forward "\n*** EOOH ***\n" end t)
	    (narrow-to-region (point) end)))
	(goto-char (point-min))
	(rmail-display-labels)
	;; 1996/12/9 by MORIOKA Tomohiko
 	(if rmail-show-mime
 	    (funcall rmail-show-mime-method)
 	  )
	(rmail-highlight-headers)
	(if (and (boundp 'transient-mark-mode) ;; check for XEmacs
		 transient-mark-mode)
	    (deactivate-mark))
	(run-hooks 'rmail-show-message-hook)
	;; If there is a summary buffer, try to move to this message
	;; in that buffer.  But don't complain if this message
	;; is not mentioned in the summary.
	;; Don't do this at all if we were called on behalf
	;; of cursor motion in the summary buffer.
	(and (rmail-summary-exists) (not no-summary)
	     (let ((curr-msg rmail-current-message))
	       (rmail-select-summary
		(rmail-summary-goto-msg curr-msg t t))))
	(if blurb
	    (message blurb))))))


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
