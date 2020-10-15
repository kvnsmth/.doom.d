;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Personal Information
(setq user-full-name "Kevin Smith"
      user-mail-address "kevin@kevinsmith.cc"
      calendar-latitude 37.8
      calendar-longitude -122.4
      calendar-location-name "San Francisco, CA")

;;; Doom Fonts
;; (setq doom-font (font-spec :family "Dank Mono" :size 32)
;;       doom-big-font (font-spec :family "Dank Mono" :size 56)
;;       doom-variable-pitch-font (font-spec :family "Overpass" :size 30))
(setq doom-font (font-spec :family "Hack" :size 32))

;;; Doom Theme
(setq doom-theme 'doom-gruvbox
      doom-gruvbox-dark-variant "hard"
      doom-gruvbox-brighter-comments t)
;; I don't like the default yellow for outline-1
;; While we're here, customize some more.
(custom-theme-set-faces! 'doom-gruvbox
  `(outline-1    :foreground ,(doom-color 'violet)       :bold t)
  `(outline-2    :foreground ,(doom-color 'blue)         :bold t)
  `(outline-3    :foreground ,(doom-color 'teal)         :bold t)
  `(outline-4    :foreground ,(doom-lighten 'red 0.2)    :bold t)
  `(outline-5    :foreground ,(doom-color 'gray)         :bold t)
  `(outline-6    :foreground ,(doom-lighten 'orange 0.4) :bold t)
  `(outline-7    :foreground ,(doom-color 'gray)         :bold t)
  `(outline-8    :foreground ,(doom-lighten 'orange 0.4) :bold t)
  `(org-ellipsis :foreground ,(doom-color 'fg)           :bold nil)
  )

;; Email
(set-email-account! "3142718"
                    '((user-mail-address     . "kevin@3142718.com")
                      (user-full-name         . "Kevin Smith")
                      (mu4e-sent-folder       . "/3142718/Sent")
                      (mu4e-drafts-folder     . "/3142718/Drafts")
                      (mu4e-trash-folder      . "/3142718/Trash")
                      (mu4e-refile-folder     . "/3142718/Archive")
                      (smtpmail-smtp-user     . "nivek@3142718.com")
                      (send-mail-function     . 'smtpmail-send-it)
                      (smtpmail-smtp-server   . "smtp.fastmail.com")
                      (smtpmail-stream-type   . ssl)
                      (smtpmail-smtp-service  . 465)
                      (mu4e-compose-signature . "\nKevin Smith"))
                    nil)
(set-email-account! "kevinsmithcc"
                    '((user-mail-address     . "kevin@kevinsmith.cc")
                      (user-full-name         . "Kevin Smith")
                      (mu4e-sent-folder       . "/kevinsmithcc/[Gmail]/Sent Mail")
                      (mu4e-drafts-folder     . "/kevinsmithcc/[Gmail]/Drafts")
                      (mu4e-trash-folder      . "/kevinsmithcc/[Gmail]/Trash")
                      (mu4e-refile-folder     . "/kevinsmithcc/[Gmail]/All Mail")
                      (smtpmail-smtp-user     . "kevin@kevinsmith.cc")
                      (send-mail-function     . 'smtpmail-send-it)
                      (smtpmail-smtp-server   . "smtp.gmail.com")
                      (smtpmail-stream-type   . ssl)
                      (smtpmail-smtp-service  . 465)
                      (mu4e-compose-signature . "\nKevin Smith"))
                    t)
(after! mu4e
  (setq mu4e-view-show-images nil
        mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask
        mu4e-compose-dont-reply-to-self t
        mu4e-split-view 'single-window
        mu4e-bookmarks '()) ; clear out default bookmarks
  (mu4e-bookmark-define
   "date:today..now" "Today" ?t)
  (mu4e-bookmark-define
   "maildir:/kevinsmithcc/Inbox OR maildir:/3142718/Inbox" "All Inboxes" ?i)
  (mu4e-bookmark-define
   "flag:flagged" "Flagged" ?f)
  (mu4e-bookmark-define
   "flag:unread AND NOT flag:trashed" "All Unread" ?u)
  )
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;;; Org Mode
;;;
(setq
 org-directory (expand-file-name "~/org/") ; main directory for all my org files
 org-roam-directory org-directory
 deft-directory org-directory

 kvnsmth-main-bib (expand-file-name "~/Nextcloud/bibliography/main.bib") ; main bibilography

 ;; helm-bibtex config
 ;; used for searching for bibliography entries
 bibtex-completion-notes-path org-directory      ; use one file per references
 bibtex-completion-bibliography kvnsmth-main-bib ; where to find main bib file
 bibtex-completion-pdf-field "file"              ; the key in entry that points to file location
 )

;; setup org-ref for citations in org
(use-package! org-ref
  :init
  (map! :after org
        :map org-mode-map
        :niv "C-n" #'org-ref-open-notes-at-point)
  :config
  (setq
   org-ref-default-bibliography kvnsmth-main-bib
   org-ref-notes-directory org-directory
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   ;; REVIEW I /think/ that orb will do this swizzling for us
   org-ref-notes-function 'orb-edit-notes
   ))

;; connect everything to org-roam
;; fyi, orb = org-roam-bibtex
(use-package! org-roam-bibtex
  :after org-roam
  :init
  (map! :after org-roam
        :map org-mode-map
        :niv "C-c n a" #'orb-note-actions)
  :config
  (setq
   orb-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords")
   ;; this is the main template for creating new note files
   orb-templates
   '(("r" "ref" plain (function org-roam-capture--get-point)
      ""
      :file-name "${slug}"
      :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}
#+roam_tags: ref

+ tags ::
+ keywords :: ${keywords}

* TODO Summary

* Notes\n:PROPERTIES:\n:Custom_ID: ${=key=}\n:URL: ${url}\n:AUTHOR: ${author-or-editor}\n:NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n:NOTER_PAGE: \n:END:\n\n"
      :unnarrowed t))
   ))
;; REVIEW Best I can tell there is some load order issue with hooking into
;; org-roam so just do it on org mode entry
(add-hook! 'org-mode-hook 'org-roam-bibtex-mode)

;; org-noter to connect pdfs and notes
(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   org-noter-always-create-frame t
   org-noter-auto-save-last-location t
   org-noter-separate-notes-from-heading t
   org-noter-default-heading-title "Page $p$"
   org-noter-notes-search-path `(,org-directory)
   )
  )

;; improve latex creation for retina displays by using SVG
(setq org-preview-latex-default-process 'dvisvgm)
;; by default do not export section numbers
(setq org-export-with-section-numbers nil)
;; less indentation to have more horizontal space
(setq org-indent-indentation-per-level 1)
;; adapt indentation, I find this more natural when adding headings
(setq org-adapt-indentation t)
;; keep blank line with cycling visibility
(setq org-cycle-separator-lines 1)
;; fancy "ellipsis" when collapsing levels
(setq org-ellipsis " ︙")

(use-package! org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(after! org-roam
  (setq
   ;; I find automatically opening the org-roam buffer annoying. disable.
   +org-roam-open-buffer-on-find-file nil
   ;; make backlinks buffer a little more narrow
   org-roam-buffer-width 0.25
   org-roam-index-file "_index.org"
   )
  )
;; set org-agenda to look at org-roam files too
(setq org-agenda-files `(,org-directory))
;; configure how org reports stuck projects
(setq org-stuck-projects '("+LEVEL=2+PROJECT-MAYBE/!-DONE-WAITING-HOLD-CANCELLED" ("NEXT") nil ""))
;; hide scheduled todos from ALL view
(setq org-agenda-todo-ignore-with-date t)
;; only show top-level todos (makes todo list more focused)
(setq org-agenda-todo-list-sublevels nil)
;; add a CLOSED timestamp property when completing todos
(setq org-log-done 'time)
;; open buffers with outline folded
(setq org-startup-folded 'content)

;; configure custom faces
(add-hook! 'doom-load-theme-hook
  (custom-declare-face '+kvnsmth-todo-active `((t (
                                                   :inherit (bold org-todo)
                                                   :foreground ,(doom-color 'base8))
                                                  )) "")
  (custom-declare-face '+kvnsmth-todo-next `((t (
                                                 :inherit (bold org-todo)
                                                 :foreground ,(doom-color 'orange)
                                                 )
                                                )) "")
  (custom-declare-face '+kvnsmth-todo-started `((t (
                                                    :inherit (bold org-todo)
                                                    :foreground ,(doom-color 'violet)
                                                    )
                                                   )) "")
  (custom-declare-face '+kvnsmth-todo-onhold `((t (
                                                   :inherit (bold org-todo)
                                                   :foreground ,(doom-color 'base4)
                                                   )
                                                  )) "")
  (custom-declare-face '+kvnsmth-todo-done '((t (
                                                 :inherit (bold success org-todo)
                                                 )
                                                )) "")
  (custom-declare-face '+kvnsmth-todo-xdone `((t (
                                                  :inherit (bold org-todo)
                                                  :foreground ,(doom-color 'teal)
                                                  )
                                                 )) "")
  )

(after! org
  ;; enable habits
  (add-to-list 'org-modules 'org-habit)
  ;; setup org roam capture templates
  (setq org-roam-capture-templates
        '(
          ("n" "note" plain #'org-roam-capture--get-point
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "permanent" plain #'org-roam-capture--get-point
           "+ tags :: %?\n\n* Note\n"
           :file-name "p_%<%Y%m%d%H%M%S>"
           :head "#+TITLE: ${title}\n#+roam_tags: perm\n\n"
           :unnarrowed nil)
          )
        )
  (setq org-roam-capture-ref-templates
        '(("w" "website" plain #'org-roam-capture--get-point
           "+ source :: ${ref}\n+ tags :: %?\n+ description :: \n\n* TODO read\n\n* Summary\n\n* Notes"
           :file-name "w_%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+roam_tags: ref\n\n"
           :unnarrowed t)
          )
        )
  (setq org-capture-templates
        '(
          ("t" "Todo" entry
           (file+headline "_todo.org" "Inbox")
           "* TODO %?\n%i\nfile:%F\n%U" :prepend t)
          ("c" "Correspondence" entry
           (file+headline "_todo.org" "Correspondence")
           "* TODO %?\n%i\n%U" :prepend t)
          ("e" "Email" entry
           (file+headline "_todo.org" "Correspondence")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("f" "Fleeting" entry
           (file+headline "_xfleeting.org" "Inbox")
           "* %U\n%?\n%i\nfile:%F" :prepend nil)
          ("g" "Gratitude" entry
           (file+olp+datetree "_gratitude.org")
           "* %?")
          ;; Link captures are for quickly storing links from my browser
          ("l" "Link" entry
           (file+olp+datetree "_links.org")
           "* %:annotation\n\"%:initial\"\n%?" :empty-lines 1)
          ;; see the doom org config.el for more capture template ideas
          ))
  ;; configure TODO states
  ;; based on http://doc.norang.ca/org-mode.html
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
  (setq org-todo-keyword-faces
        '(("TODO"      . +kvnsmth-todo-active)
          ("NEXT"      . +kvnsmth-todo-next)
          ("STARTED"   . +kvnsmth-todo-started)
          ("DONE"      . +kvnsmth-todo-done)
          ("WAITING"   . +kvnsmth-todo-onhold)
          ("HOLD"      . +kvnsmth-todo-onhold)
          ("CANCELLED" . +kvnsmth-todo-xdone)
          ("PHONE"     . +kvnsmth-todo-xdone) ; REVIEW I might not end up using this one much
          ("MEETING"   . +kvnsmth-todo-xdone)))
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("STARTED" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  (setq org-agenda-start-day "0d"
        org-agenda-span 3)
  (setq org-agenda-tags-column 80)
  (setq org-habit-preceding-days 7
        org-habit-following-days 3
        org-habit-graph-column 80
        org-habit-show-done-always-green t
        org-habit-show-habits-only-for-today t
        org-habit-show-all-today t)
  ;; I don't like the auto-resizing thing doom does
  (remove-hook 'org-agenda-mode-hook #'+org-habit-resize-graph-h)
  ;; explicitly load org modules
  (org-load-modules-maybe t)
  )

;; customized agenda view
;; inspired by https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))
(setq org-agenda-custom-commands
      '(("k" "Kustom Agenda"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Unfinished tasks:")))
          (agenda "")
          (stuck ""
                 ((org-agenda-overriding-header "Stuck projects:")))
          (alltodo ""
                   ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'regexp ":PROJECT:")
                                                   (air-org-skip-subtree-if-priority ?A)))
                    (org-agenda-overriding-header "Unscheduled tasks:")
                    (org-agenda-sorting-strategy '((todo
                                                    user-defined-down
                                                    priority-down
                                                    category-keep))))))
         ((org-agenda-block-separator "~~~~")))))

;; custom sorting based on todo state
;; I want NEXT and WAITING to appear at the top of agenda sorting
(defun kls-compare-todo-state (a b)
  (let (
        (tsa (org-entry-get (get-text-property 0 'org-hd-marker a) "TODO"))
        (tsb (org-entry-get (get-text-property 0 'org-hd-marker b) "TODO"))
        (order (list "NEXT" "WAITING" "TODO"))
        )
    (if (< (seq-position order tsa) (seq-position order tsb))
        +1 -1)))
(setq org-agenda-cmp-user-defined #'kls-compare-todo-state)

;; configure pretty bullets
(after! org-superstar
  (setq org-superstar-headline-bullets-list
        '("◉" "○")) ; I prefer alternating bullets
  (setq org-superstar-item-bullet-alist
        '((?* . ?∘)
          (?+ . ?＋)
          (?- . ?－)))
  )

;; tweak autocomplete with comapny so that it appears quickly
(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-minimum-width 30
        company-tooltip-maximum-width 60)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; projectile setup
(setq projectile-project-search-path '("~/code/" "~/code/kvnsmth/"))

;; switch to new window after a split
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; use gravatars for commits
(setq magit-revision-show-gravatars t)
;; treemacs config
(setq +treemacs-git-mode nil      ; I don't need git in my treemacs
      treemacs-position 'right    ; personal preference
      treemacs-silent-filewatch t ; shh
      treemacs-silent-refresh t   ; shh
      treemacs-resize-icons 44)   ; hidpi goodness

;; tweak some defaults
(setq-default
 ;; REVIEW turn off resizing behavior bc I think it might be causing issues
 ;; with SPC w o
 ;; window-combination-resize t ; take space from all windows when splitting
 x-stretch-cursor t          ; take up entire space of glyph
 )

;;; General Config
(setq
 auto-save-default t                 ; auto save!
 truncate-string-ellipsis "…"        ; lets use the real ellipsis character, looks nicer
 mac-command-modifier 'meta          ; use command, instead of option, for meta
 mac-option-modifier  'super         ; swap super to option (usually on command)
 global-auto-revert-mode t           ; update buffers if files change outside emacs
 display-line-numbers-type 'relative ; relative makes motion easier to calc
 )

;; auto break lines of text when writing
(add-hook 'text-mode-hook 'auto-fill-mode)
;; what does this do?
(setq display-fill-column-indicator t)

;; convenience functions to undo fill mode
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;;
;; keymappings
;;
;; maintain comment behavior with meta, since we remapped command keeping
(map! :nie "M-/" #'comment-dwim)
;; similar but for pasting (old habits die hard)
(map! :g "M-v" #'yank)

;; lets make which key show up quickly!
(setq which-key-idle-delay 0.5)

;; let avy search across all windows
(setq avy-all-windows t)

;; center text when visual wrapping is on
(setq visual-fill-column-center-text t)

;; made spelling and grammar opt-in
;; (remove-hook! '(org-mode-hook markdown-mode-hookk)
;;   #'flyspell-mode)
;; (remove-hook! '(org-mode-hook markdown-mode-hook)
;;   #'writegood-mode)
;; (add-hook! 'org-mode-hook (flycheck-mode -1))

(setq ispell-dictionary "en")

;; company backends don't load reliably for org
;; I don't know why but text mode seemed to be hijacking company-org-roam
;; REVIEW surely this is a bug...
(set-company-backend! '(org-mode text-mode) '(company-org-roam company-yasnippet company-dabbrev))

;;
;; package configs
;;

;; simpler minimal writing environment
(use-package! olivetti
  :config
  (setq-default olivetti-body-width 110)
  :defer-incrementally t)

;; solaire-mode adds some nice dimming effects to the interface
;; you can see it particularly with treemacs
(use-package! solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

;; configure prettier
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(use-package! prettier-js)
(add-hook! 'js2-mode-hook 'prettier-js-mode)
;; only enable for JSX in web mode
(add-hook! 'web-mode-hook #'(lambda ()
                              (enable-minor-mode
                               '("\\.jsx?\\'" . prettier-js-mode)))
           )

;; vlf helps load REALLY large files. it will prompt to use.
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
