(require 's)
(require 'dash)

(defgroup org-remembrance nil
  "Remembrance Agents for Org Mode"
  :group 'org
  :prefix "org-remembrance-")

(defcustom org-remembrance-max-results 5
  "This is the number of results to be displayed per page."
  :group 'org-remembrance
  :type 'integer)

(defcustom org-remembrance-buffer "*org-remembrance*"
  "This is the buffer name for org-remembrance."
  :group 'org-remembrance
  :type 'string)

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
    (insert (string-trim (shell-command-to-string (concat "recoll -o -t -N -F \"url abstract filename\" '" query "' 2> /dev/null"))))
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
  (concat (format "* [[%s][%s]]"
                  (cdr (assoc "url" result))
                  (cdr (assoc "filename" result)))
          "\n\n"
          (s-collapse-whitespace (cdr (assoc "abstract" result)))
          "\n"))

;;;###autoload
(defun org-remembrance-update-results (&optional query)
  (interactive)
  (unless query
    (setq query (sentence-at-point)))
  (with-current-buffer (get-buffer-create org-remembrance-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (org-mode)
    (make-local-variable 'org-return-follows-link)
    (setq org-return-follows-link t)
    (org-remembrance-mode +1)
    (let ((results (org-remembrance-get-results query)))
      (if results
          (progn
            (insert (s-join "\n" (-map #'org-remembrance-format-result results)))
            (highlight-phrase query 'hi-yellow))
        (insert "No results")))
    (read-only-mode +1)))
