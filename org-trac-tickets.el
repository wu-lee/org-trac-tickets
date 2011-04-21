;; Functions for interoperating with Trac

(require 'url)
(require 'cl)
(require 'org-mode)

; This must be set for anything to work.
; For example: "http://trac.mydomain.com/projects/foo-project/trac"
(defvar org-trac-base-url 
  "Specifies the base URL of the tract project you interoperate with"
  nil)


(defun* org-trac-create-ticket (user summary description)
  "This will create a ticket in the repository with the given name and
description, reported by user.  It returns the ticket ID (as a
string)."
  (let* ((new-ticket-url (concat org-trac-base-url "/newticket"))
	 (login-url (concat org-trac-base-url "/login"))
	 (login-response-buffer (url-retrieve-synchronously login-url))
	 (response-buffer1 (url-retrieve-synchronously new-ticket-url))
	 (form-token (save-excursion
		       (set-buffer response-buffer1)
		       (re-search-forward "name=\"__FORM_TOKEN\" value=\"\\(.*?\\)\"" nil t)
		       (match-string 1)))
	 (url-request-method "POST")
	 (post-data `(("field_reporter" . ,user)
		      ("field_summary" . ,summary)
		      ("field_description" . ,description)
		      ("submit" . "Create ticket")
		      ("__FORM_TOKEN" . ,form-token)))
	 (url-request-extra-headers
	  '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data 
	  (mapconcat (lambda (post-data)
		       (concat (url-hexify-string (car post-data))
			       "="
			       (url-hexify-string (cdr post-data))))
		     post-data "&"))
	 (response-buffer2 (url-retrieve-synchronously new-ticket-url))
	 (new-ticket-id (save-excursion
			  (set-buffer response-buffer2)
			  (re-search-forward "Add/Change #\\([0-9]+\\)" nil t)
			  (match-string 1))))
   
    (kill-buffer login-response-buffer)
    (kill-buffer response-buffer1)
    (kill-buffer response-buffer2)
    new-ticket-id
    ))



; from https://github.com/jave/inkmacs/blob/master/inkmacs.el
(defun my-org-get-entry ()
  "Get the entry text, after heading, to nex heading, or eof."
  (save-excursion
    (org-back-to-heading t)
    (let ((p1 (point-at-bol 2))
          (p2 (progn (forward-line) (search-forward "*" nil t))))
      (setq p2 (if (null p2) (point-max)
                 (1- p2)))
      (buffer-substring-no-properties p1 p2))))

(defun my-org-join-components (delim &rest components)
  "Given components, remove nils, and concatenate the rest together
with the delimiter delim."
  (mapconcat 'identity (remove-if 'null components) delim))

(defun my-org-heading-components ()
  "A customised version of org-heading-components which more reliably
sets the return fields.  Specifically, the 4th and 5th will always
be the headline and the tags, even if there are tags but no
headline."

  (save-excursion
    (org-back-to-heading t)
    (when (let (case-fold-search) (looking-at org-complex-heading-regexp))
      (let ((stars    (match-string 1))
	    (todo     (match-string 2))
	    (priority (match-string 3))
	    (headline (match-string 4))
	    (tags     (match-string 5)))
	(if (and (null tags) 
		 (not (null headline)) 
		 (string-match-p "^:.*:$" headline))
	    (list stars todo priority nil headline) ; tags are in headline
	  (list stars todo priority headline tags))))))


(defun my-org-set-headline (text) 
  "Set the current entry's headline to the value of text"
  (let ((components (my-org-heading-components)))
    (unless (null components)
      (save-excursion
	(destructuring-bind 
	    (stars todo priority headline tags)
	    components
	  (replace-match (my-org-join-components " " stars todo priority text tags) t t))))))

(defun org-trac-create-ticket-from-entry ()
  "An interactive function which will create a ticket from the org entry at 
point, and update the entry's heading to include the ticket number."
  (interactive)
  (destructuring-bind 
      (level reduced-level todo priority headline tags)
      (org-heading-components)
    (when (and (null tags) (> (length headline) 0) (string= (substring headline 0 1) ":"))
      (setq headline ""))
    (if (string-match "\\([^ \t]+\\)?#\\([0-9]+\\)" headline)
	(message "headline refers to ticket #%d already" (match-string 0))
      (let* ((description (my-org-get-entry))
	     (project "") ; for now
	     (user (cond
		    ((boundp 'org-trac-user) org-trac-user)
		    ((getenv "USER"))
		    (error "Cannot determing user name. Perhaps you should set org-trac-user.")))
	     (new-ticket-id (org-trac-create-ticket user headline description)))
	(my-org-set-headline (format "%s#%s: %s" project new-ticket-id headline))
	(message "created ticket %s#%s: %s" project new-ticket-id headline) ))))

; (org-trac-create-ticket "nick" "testing" "ignore this")

