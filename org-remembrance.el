(require 's)
(require 'dash)

(defgroup org-remembrance nil
  "Remembrance Agents for Org Mode"
  :group 'org
  :prefix "org-remembrance-")

(defcustom org-remembrance-window-size 3
  "Sliding window size for query."
  :group 'org-remembrance
  :type 'integer)

(defcustom org-remembrance-idle-delay 3
  "Delay before updating results."
  :group 'org-remembrance
  :type 'integer)

(defcustom org-remembrance-max-results 5
  "This is the number of results to be displayed per page."
  :group 'org-remembrance
  :type 'integer)

(defcustom org-remembrance-buffer "*org-remembrance*"
  "This is the buffer name for org-remembrance."
  :group 'org-remembrance
  :type 'string)

(defvar org-remembrance-reload-timer nil
  "Timer for autoreload.")

(defvar org-remembrance-query nil
  "Current query for org-remembrance.")

(defvar org-remembrance-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "n") 'org-next-link)
    (define-key kmap (kbd "p") 'org-previous-link)
    (define-key kmap (kbd "q") 'delete-window)
    kmap)
  "The keymap used for `org-remembrance-mode'.")

(define-minor-mode org-remembrance-mode
  "A minor mode to simplify navigation of recoll search results."
  nil
  :lighter " org-remembrance" ; modeline notice
  :keymap org-remembrance-mode-map ; key bindings
  :group 'org-remembrance)

(defun org-remembrance-search (&optional query)
  "Prompt for a QUERY and search."
  (interactive)
  (unless query
    (setq query (org-remembrance-completing-read)))
  (org-remembrance-update-results query))


(defun org-remembrance-get-results (query)
  (with-temp-buffer
    (let ((command (format "recoll -o -t -N -F \"\" -n %i '%s' 2> /dev/null" org-remembrance-max-results query)))
      (insert (string-trim (shell-command-to-string command))))
    (goto-char (point-min))
    (kill-line 2)
    (if (= (buffer-size (current-buffer)) 0)
        '()
      (-map (lambda (line)
              (setq items (s-split " " (s-trim line)))
              (setq al (list))
              (while items
                (let ((key (car items))
                      (value (base64-decode-string (car (cdr items)))))
                  (add-to-list 'al (cons key value)))
                (setq items (cdr (cdr items))))
              al)
            (s-lines (buffer-string))))))

(defun org-remembrance-format-result (result)
  (concat (format "* [[%s][%s]] %s"
                  (cdr (assoc "url" result))
                  (or (cdr (assoc "filename" result))
                      (cdr (assoc "title" result)))
                  (cdr (assoc "relevancyrating" result)))
          "\n\n"
          (s-collapse-whitespace (or (cdr (assoc "abstract" result)) "No abstract."))
          "\n"))

(defun org-remembrance-split-and-focus ()
  (interactive)
  "Split window and focus the remembrance results window after an original search."
  (when (= (length (window-list)) 1)
    (split-window-right))
  (other-window 1)
  (switch-to-buffer org-remembrance-buffer))

(defun org-remembrance-get-window ()
  (save-excursion
    (backward-word org-remembrance-window-size)
    (setq start (point))
    (forward-word (+ 1 (* 2 org-remembrance-window-size)))
    (setq end (point))
    (buffer-substring-no-properties start end)))

;;;###autoload
(defun org-remembrance-update-results (&optional query)
  (interactive)
  (unless query
    (setq query (or (when (use-region-p)
                      (buffer-substring (region-beginning) (region-end)))
                    (org-remembrance-get-window)
                    "")))
  (when query
    (setq query (s-downcase (s-replace "'" "\\'" (s-collapse-whitespace query)))))
  (unless (s-equals? org-remembrance-query query)
    (with-current-buffer (get-buffer-create org-remembrance-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (make-local-variable 'org-return-follows-link)
      (setq org-return-follows-link t)
      (org-remembrance-mode +1)
      (insert (format "Query: %s\n\n" query))
      (let ((results (org-remembrance-get-results query)))
        (if results
            (progn
              (insert (s-join "\n" (-map #'org-remembrance-format-result results)))
              (-map (lambda (phrase)
                      (highlight-phrase phrase 'bold-italic)
                      ) (s-split " " query)))
          (insert "No results")))
      (read-only-mode +1))))

;;;###autoload
(defun org-remembrance-autoreload ()
  (interactive)
  (setq org-remembrance-reload-timer (run-with-idle-timer org-remembrance-idle-delay t
                                                          (lambda ()
                                                            (when (and (eq major-mode 'org-mode)
                                                                       (get-buffer-window org-remembrance-buffer))
                                                              (org-remembrance-update-results))))))

;;;###autoload
(defun org-remembrance-autoreload-cancel ()
  (interactive)
  (when org-remembrance-reload-timer
    (cancel-timer org-remembrance-reload-timer)
    (setq org-remembrance-reload-timer nil)))
