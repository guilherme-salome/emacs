;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Load personal information
(let ((personal-file (expand-file-name "personal.el" doom-user-dir)))
     (when (file-exists-p personal-file)
       (load personal-file)))

;; Prefer encrypted authentication source
(setq! auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Basic MacOS adjusments
(set-frame-parameter nil 'fullscreen 'maximized)
(setq! doom-theme 'doom-fairy-floss
       display-line-numbers-type t
       mac-command-modifier 'meta
       mac-option-modifier 'meta
       user-full-name personal-name
       user-mail-address personal-email
       bookmark-default-file (expand-file-name "bookmarks" personal-folder)
       +snippets-dir (concat personal-folder "yasnippet/"))

;; fonts
;(set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 1)
;(set-face-attribute 'default nil :font "Iosevka" :weight 'regular :height 160)

;; Tramp
(after! tramp
  (setq! tramp-inline-compress-start-size 1000
         tramp-copy-size-limit 10000
         vc-handled-backends '(Git)
         tramp-verbose 1
         tramp-default-method "scp"
         tramp-use-ssh-controlmaster-options nil
         projectile--mode-line "Projectile"
         tramp-verbose 1))

;; Org-mode configurations
(after! org
  (setq! org-agenda-files (list (concat personal-folder "roam/lilly/main.org"))
         org-log-done 'note
         org-todo-keywords '((sequence "TODO(t)" "WORKING(w)" "WAITING(h)" "REVIEW(r)" "|" "DONE(d)" "CANCELED(c)"))
         org-todo-keyword-faces '(("TODO" . (:foreground "cyan"))
                                  ("WORKING" . (:foreground "royal blue"))
                                  ("WAITING" . (:foreground "orange"))
                                  ("REVIEW" . (:foreground "navy"))
                                  ("CANCELED" . (:foreground "black"))
                                  ("DONE" . (:foreground "light sea green")))
         org-agenda-start-on-weekday 7
         org-agenda-span 90
         org-agenda-prefix-format '((agenda . "%?-12t% s"))
         org-agenda-sorting-strategy '(deadline-up)
         org-deadline-warning-days 30
         org-agenda-compact-blocks t
         org-agenda-format-date (lambda (date) (;; If date is SATURDAY then add a line, otherwise just use the default format
                                                if (eq (calendar-day-of-week date) 1)
                                                (concat
                                                 (make-string (window-width) 9472)
                                                 "\n"
                                                 (org-agenda-format-date-aligned date))
                                                (org-agenda-format-date-aligned date)))
         org-export-babel-evaluate nil
         org-hide-emphasis-markers t
         org-format-latex-options (plist-put org-format-latex-options :scale 2))
         org-capture-templates '(("w" "Weekly report" entry
                                  (file+olp "~/Library/CloudStorage/GoogleDrive-guilhermesalome@gmail.com/My Drive/Emacs/roam/lilly.org" "Progress")
                                  "** %<%G>-W%<%V>\n*** Highlights\n*** Time Summary\n#+BEGIN: clocktable :block thisweek :maxlevel 3 :formula % :compact t\n#+END:"))
         org-tag-alist
         '(("meeting" . ?m) ("coaching" . ?c) ("code" . ?x) ("writing" . ?w) ("admin" . ?a))
         org-use-tag-inheritance t
         org-tags-exclude-from-inheritance '("ARCHIVE" "noexport")
  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t)))
  (with-eval-after-load 'org
    (setq org-log-done 'time))
  (with-eval-after-load 'org-clock
    (setq org-clock-into-drawer t
          org-clock-persist 'history)
    (org-clock-persistence-insinuate))
  (with-eval-after-load 'org-duration
    (setq org-duration-format 'h:mm))
  )

;; Hugo (blogging)
(after! ox
  (setq! org-hugo-base-dir personal-blog-folder
         org-hugo-default-section-directory "blog"
         org-hugo-auto-set-lastmod t))

;; Helper function to obtain credentials for LLM APIs
(defun get-llm-api-key (host)
  "Return the API key for HOST from auth-source (~/.authinfo.gpg). Returns nil if no entry found."
  (let* ((entry (car (auth-source-search :host host :max 1 :require '(:secret)))))
    (when entry
      (let ((secret (plist-get entry :secret)))
        (if (functionp secret) (funcall secret) secret)))))

;; gptel (LLMs)
(use-package! gptel
  :config
  (setq! gptel-api-key (get-llm-api-key "api.openai.com")
         gptel-default-mode 'org-mode)
  (gptel-make-anthropic "Claude" :stream t :key (get-llm-api-key "api.anthropic.com")))

;; whisper (transcription model)
(defun rk/get-ffmpeg-device ()
  "Gets the list of devices available to ffmpeg.
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio devices.
Each list contains a list of cons cells, where the car is the device number and the cdr is the device name."
  (unless (string-equal system-type "darwin")
    (error "This function is currently only supported on macOS"))

  (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
    (cl-loop with at-video-devices = nil
             with at-audio-devices = nil
             with video-devices = nil
             with audio-devices = nil
             for line in lines
             when (string-match "AVFoundation video devices:" line)
             do (setq at-video-devices t
                      at-audio-devices nil)
             when (string-match "AVFoundation audio devices:" line)
             do (setq at-audio-devices t
                      at-video-devices nil)
             when (and at-video-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
             when (and at-audio-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
             finally return (list (nreverse video-devices) (nreverse audio-devices)))))

(defun rk/find-device-matching (string type)
  "Get the devices from `rk/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
  (let* ((devices (rk/get-ffmpeg-device))
         (device-list (if (eq type :video)
                          (car devices)
                        (cadr devices))))
    (cl-loop for device in device-list
             when (string-match-p string (cdr device))
             return (car device))))

(defcustom rk/default-audio-device nil
  "The default audio device to use for whisper.el and outher audio processes."
  :type 'string)

(defun rk/select-default-audio-device (&optional device-name)
  "Interactively select an audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
  (interactive)
  (let* ((audio-devices (cadr (rk/get-ffmpeg-device)))
         (indexes (mapcar #'car audio-devices))
         (names (mapcar #'cdr audio-devices))
         (name (or device-name (completing-read "Select audio device: " names nil t))))
    (setq rk/default-audio-device (rk/find-device-matching name :audio))
    (when (boundp 'whisper--ffmpeg-input-device)
      (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device)))))

(defun rk/whisper-run (prefix-arg)
  "Run whisper, prompting for audio device if needed.
With PREFIX-ARG (C-u), always prompt for a new device."
  (interactive "P")
  (when (or prefix-arg (not (and (boundp 'whisper--ffmpeg-input-device)
                                 whisper--ffmpeg-input-device)))
    (call-interactively #'rk/select-default-audio-device))
  ;; DO NOT call interactively, just call as function,
  ;; so no prefix arg is passed down
  (funcall #'whisper-run))

(defun rk/save-whisper-audio-clip ()
  (let* ((archive-name (format-time-string "%Y%m%d%H%M%S.wav"))
         (archive-file (file-name-concat personal-whisper-folder "recordings" archive-name)))
    (make-directory (file-name-directory archive-file) t)
    (copy-file whisper--temp-file archive-file)))

(defvar whisper--ffmpeg-input-device nil)
(use-package! whisper
  :bind (("C-c m r" . rk/whisper-run)   ; calls for audio-device first
         ("C-c m d" . rk/select-default-audio-device)
         ("C-c m w" . whisper-run))
  :config
  (setq! whisper-install-directory personal-whisper-folder
         whisper-model "medium"
         whisper-language "auto"
         whisper-translate t
         whisper-use-threads (/ (num-processors) 2)
         whisper-insert-text-at-point nil
         whisper-recording-timeout 7200)
  (add-hook 'whisper-after-transcription-hook #'rk/save-whisper-audio-clip 100))

;; Github Copilot
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; org-re-reveal (presentations using revealjs)
(use-package! emacs-reveal
  :after org
  :init (setq! emacs-reveal-managed-install-p nil)
  :config (setq! org-re-reveal-revealjs-version "4"
                 org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js/"))

;; Org-roam
(use-package! org-roam
  :config
  (setq! org-roam-directory (concat personal-folder "roam/")
         org-roam-dailies-directory "daily/"
         org-roam-capture-templates
         '(("d" "default" plain "%?"
            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n")
            :unnarrowed t)
           ("lp" "lilly-person" plain "%?"
            :target (file+head "lilly/people/${slug}.org"
                               "#+title: ${title}\n")
            :unnarrowed t)
           ("ld" "lilly-document" plain "%?"
            :target (file+head "lilly/docs/${slug}.org"
                               "#+title: ${title}\n")
            :unnarrowed t))))

;; org-special-block-extras
(use-package! org-special-block-extras
  :config
  (org-special-block-extras-mode)
  (org-defblock src (lang "" title nil)
                "Fold-away all ‘src’ blocks as ‘<details>’ in HTML export.
`'   For Hugo (Markdown) export, format as a Markdown fenced code block.
   Other backends use default Org-mode export."
                (cond
                 ;; Hugo Export: Format as a fenced Markdown code block
                 ((org-export-derived-backend-p org-export-current-backend 'hugo)
                  (format "```%s\n%s\n```" lang raw-contents))
                 ;; HTML Export: Use collapsible <details>
                 ((org-export-derived-backend-p org-export-current-backend 'html)
                  (format "<details><summary>%s</summary><pre>%s</pre></details>"
                          (or title (concat "Code (" lang ")")) raw-contents))
                 ;; Default Export: Use Org-mode's default behavior
                 (t raw-contents))))

;; org-present
(use-package! org-present
  :after org)

;; visual-fill-column
(use-package! visual-fill-column
  :after org
  :config
  (setq! visual-fill-column-width 110
         visual-fill-column-center-text t)
  (defun my/org-present-start ()
      (visual-fill-column-mode 1)
      (org-redisplay-inline-images))
  (defun my/org-present-end ()
    (visual-fill-column-mode 0))
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end))

;; org-ql
(use-package! org-ql)
