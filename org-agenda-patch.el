(defun org-agenda-get-deadlines (&optional with-hour)
  "Return the deadline information for agenda display.
When WITH-HOUR is non-nil, only return deadlines with an hour
specification like [h]h:mm."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (if with-hour
		     org-deadline-time-hour-regexp
		   org-deadline-time-regexp))
	 (today (org-today))
	 (today? (org-agenda-today-p date)) ; DATE bound by calendar.
	 (current (calendar-absolute-from-gregorian date))
	 deadline-items)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(unless (save-match-data (org-at-planning-p)) (throw :skip nil))
	(org-agenda-skip)
	(let* ((s (match-string 1))
	       (pos (1- (match-beginning 1)))
	       (todo-state (save-match-data (org-get-todo-state)))
	       (done? (member todo-state org-done-keywords))
               (sexp? (string-prefix-p "%%" s))
	       ;; DEADLINE is the deadline date for the entry.  It is
	       ;; either the base date or the last repeat, according
	       ;; to `org-agenda-prefer-last-repeat'.
	       (deadline
		(cond
		 (sexp? (org-agenda--timestamp-to-absolute s current))
		 ((or (eq org-agenda-prefer-last-repeat t)
		      (member todo-state org-agenda-prefer-last-repeat))
		  (org-agenda--timestamp-to-absolute
		   s today 'past (current-buffer) pos))
		 (t (org-agenda--timestamp-to-absolute s))))
	       ;; REPEAT is the future repeat closest from CURRENT,
	       ;; according to `org-agenda-show-future-repeats'. If
	       ;; the latter is nil, or if the time stamp has no
	       ;; repeat part, default to DEADLINE.
	       (repeat
		(cond
		 (sexp? deadline)
		 ((<= current today) deadline)
		 ((not org-agenda-show-future-repeats) deadline)
		 (t
		  (let ((base (if (eq org-agenda-show-future-repeats 'next)
				  (1+ today)
				current)))
		    (org-agenda--timestamp-to-absolute
		     s base 'future (current-buffer) pos)))))
	       (diff (- deadline current))
	       (suppress-prewarning
		(let ((scheduled
		       (and org-agenda-skip-deadline-prewarning-if-scheduled
			    (org-entry-get nil "SCHEDULED"))))
		  (cond
		   ((not scheduled) nil)
		   ;; The current item has a scheduled date, so
		   ;; evaluate its prewarning lead time.
		   ((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
		    ;; Use global prewarning-restart lead time.
		    org-agenda-skip-deadline-prewarning-if-scheduled)
		   ((eq org-agenda-skip-deadline-prewarning-if-scheduled
			'pre-scheduled)
		    ;; Set pre-warning to no earlier than SCHEDULED.
		    (min (- deadline
			    (org-agenda--timestamp-to-absolute scheduled))
			 org-deadline-warning-days))
		   ;; Set pre-warning to deadline.
		   (t 0))))
	       (wdays (if suppress-prewarning
			  (let ((org-deadline-warning-days suppress-prewarning))
			    (org-get-wdays s))
        (org-get-wdays s)))

        (deadlines (org-entry-get nil "DEADLINE")) ; ugh fucking ridiculous
        (rdeadline (if deadlines (org-get-repeat deadlines) nil))
        (rrepeat (if rdeadline (substring rdeadline 0 2) nil))
        ; fucking hell, I can't even match agains regex, wtf????
        (rdeadline-every (if rrepeat (not (or (string= rrepeat "++") (string= rrepeat ".+"))) nil))
        (scheduleds (org-entry-get nil "SCHEDULED"))
        (scheduled (if scheduleds (org-agenda--timestamp-to-absolute scheduleds) nil))
        )
	  (cond
     ((and rdeadline-every scheduled deadline (<= deadline scheduled)) (throw :skip nil))
	   ;; Only display deadlines at their base date, at future
	   ;; repeat occurrences or in today agenda.
	   ((= current deadline) nil)
	   ((= current repeat) nil)
	   ((not today?) (throw :skip nil))
	   ;; Upcoming deadline: display within warning period WDAYS.
	   ((> deadline current) (when (> diff wdays) (throw :skip nil)))
	   ;; Overdue deadline: warn about it for
	   ;; `org-deadline-past-days' duration.
	   (t (when (< org-deadline-past-days (- diff)) (throw :skip nil))))
	  ;; Possibly skip done tasks.
	  (when (and done?
		     (or org-agenda-skip-deadline-if-done
			 (/= deadline current)))
	    (throw :skip nil))
	  (save-excursion
	    (re-search-backward "^\\*+[ \t]+" nil t)
	    (goto-char (match-end 0))
	    (let* ((category (org-get-category))
		   (level (make-string (org-reduced-level (org-outline-level))
				       ?\s))
		   (head (buffer-substring (point) (line-end-position)))
		   (inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
			(and (listp org-agenda-show-inherited-tags)
			     (memq 'agenda org-agenda-show-inherited-tags))
			(and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
				 (memq 'agenda
				       org-agenda-use-tag-inheritance)))))
		   (tags (org-get-tags-at nil (not inherited-tags)))
		   (time
		    (cond
		     ;; No time of day designation if it is only
		     ;; a reminder.
		     ((and (/= current deadline) (/= current repeat)) nil)
		     ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		      (concat (substring s (match-beginning 1)) " "))
		     (t 'time)))
		   (item
		    (org-agenda-format-item
		     ;; Insert appropriate suffixes before deadlines.
		     ;; Those only apply to today agenda.
		     (pcase-let ((`(,now ,future ,past)
				  org-agenda-deadline-leaders))
		       (cond
			((and today? (< deadline today)) (format past (- diff)))
			((and today? (> deadline today)) (format future diff))
			(t now)))
		     head level category tags time))
		   (face (org-agenda-deadline-face
			  (- 1 (/ (float diff) (max wdays 1)))))
		   (upcoming? (and today? (> deadline today)))
		   (warntime (get-text-property (point) 'org-appt-warntime)))
	      (org-add-props item props
		'org-marker (org-agenda-new-marker pos)
		'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		'warntime warntime
		'level level
		'ts-date deadline
		'priority
		;; Adjust priority to today reminders about deadlines.
		;; Overdue deadlines get the highest priority
		;; increase, then imminent deadlines and eventually
		;; more distant deadlines.
		(let ((adjust (if today? (- diff) 0)))
		  (+ adjust (org-get-priority item)))
		'todo-state todo-state
		'type (if upcoming? "upcoming-deadline" "deadline")
		'date (if upcoming? date deadline)
		'face (if done? 'org-agenda-done face)
		'undone-face face
		'done-face 'org-agenda-done)
	      (push item deadline-items))))))
    (nreverse deadline-items)))
