;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Personal Information
(setq user-full-name "Kevn Smith"
      user-mail-address "kevin@kevinsmith.cc"
      calendar-latitude 37.8
      calendar-longitude -122.4
      calendar-location-name "San Francisco, CA")

;;; Doom Fonts
(setq doom-font (font-spec :family "Dank Mono" :size 15)
      doom-big-font (font-spec :family "Dank Mono" :size 28)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 15))

;;; Doom Theme
(setq doom-theme 'doom-peacock)

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
;; Best I can tell there is some load order issue with hooking into
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

;; by default do not export section numbers
(setq org-export-with-section-numbers nil)
;; less indentation to have more horizontal space
(setq org-indent-indentation-per-level 1)
;; disable automatic indentation
(setq org-adapt-indentation nil)
;; keep blank line with cycling visibility
(setq org-cycle-separator-lines 1)
;; fancy ellipsis when collapsing levels
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
;; sort diary entries
(add-hook! 'diary-list-entries-hook 'diary-sort-entries)
;; include diary entries by default
(setq org-agenda-include-diary t)
;; set org-agenda to look at org-roam files too
(setq org-agenda-files `(,org-directory))
;; configure how org reports stuck projects
(setq org-stuck-projects '("+LEVEL=1+PROJECT-MAYBE/!-DONE-WAITING-HOLD-CANCELLED" ("NEXT") nil ""))
;; hide scheduled todos from ALL view
(setq org-agenda-todo-ignore-with-date t)
;; only show top-level todos (makes todo list more focused)
(setq org-agenda-todo-list-sublevels nil)
;; add a CLOSED timestamp property when completing todos
(setq org-log-done 'time)
;; open buffers with outline folded
(setq org-startup-folded t)

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
          ("f" "Fleeting" entry
           (file+headline "_xfleeting.org" "Inbox")
           "* %U\n%?\n%i\nfile:%F" :prepend t)
          ("g" "Gratitude" entry
           (file+olp+datetree "_gratitude.org")
           "* %?")
          ;; see the doom org config.el for more capture template ideas
          ))
  ;; configure TODO states
  ;; based on http://doc.norang.ca/org-mode.html
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
  (setq org-todo-keyword-faces
        '(("TODO"      . +kvnsmth-todo-active)
          ("NEXT"      . +kvnsmth-todo-next)
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
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  )

;; configure pretty bullets
(after! org-superstar
  (setq org-superstar-headline-bullets-list
        '("◉" "○")) ; I prefer alternating bullets
  (setq org-superstar-item-bullet-alist
        '((?* . ?∘)
          (?+ . ?＋)
          (?- . ?－)))
  )

;; Relative line numbers
(setq display-line-numbers-type 'relative)

;; tweak autocomplete with comapny so that it appears quickly
(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; projectile setup
(setq projectile-project-search-path '("~/code/" "~/code/kvnsmth/"))

;; switch to new window after a slit
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; start frame maximized
(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; use gravatars for commits
(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;; tweak some defaults
(setq-default
 ;; REVIEW turn off resizing behavior bc I think it might be causing issues
 ;; with SPC w o
 ;; window-combination-resize t ; take space from all windows when splitting
 x-stretch-cursor t          ; take up entire space of glyph
 )

;;; General Config
(setq
 auto-save-default t          ; auto save!
 truncate-string-ellipsis "…" ; lets use the real ellipsis character, looks nicer
 mac-command-modifier 'meta   ; use command, instead of option, for meta
 mac-option-modifier  'super  ; swap super to option (usually on command)
 global-auto-revert-mode t    ; update buffers if files change outside emacs
 )

;; auto break lines of text when writing
(add-hook 'text-mode-hook 'auto-fill-mode)

;;
;; keymappings
;;
;; maintain comment behavior with meta, since we remapped command keeping
(map! :nie "M-/" #'comment-dwim)
;; similar but for pasting (old habits die hard)
(map! :g "M-v" #'yank)

;; lets make which key show up quick!
(setq which-key-idle-delay 0.5)

;; let avy search across all windows
(setq avy-all-windows t)

;; center text when visual wrapping is on
(setq visual-fill-column-center-text t)

;; made spelling and grammar opt-in
(remove-hook! '(org-mode-hook markdown-mode-hookk)
  #'flyspell-mode)
(remove-hook! '(org-mode-hook markdown-mode-hook)
  #'writegood-mode)
(add-hook! 'org-mode-hook (flycheck-mode -1))

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
  (setq-default olivetti-body-width 90)
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
(add-hook! 'web-mode-hook #'(labmda ()
                                    (enable-minor-mode
                                     '("\\.jsx?\\'" . prettier-js-mode)))
           )

;; vlf helps load REALLY large files. it will prompt to use.
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; improve info colors
(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)

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
