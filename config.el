;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kevn Smith"
      user-mail-address "kevin@kevinsmith.cc")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Dank Mono" :size 15)
      doom-big-font (font-spec :family "Dank Mono" :size 28)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;;
;; configure org-mode
;;
(setq org-directory (expand-file-name "~/org/")
      org-roam-directory (expand-file-name "~/org/roam/"))
;; by default do not export section numbers
(setq org-export-with-section-numbers nil)
;; I find automatically opening the org-roam buffer annoying. disable.
(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil))
;; sort diary entries
(add-hook! 'diary-list-entries-hook 'diary-sort-entries)
;; include diary entries by default
(setq org-agenda-include-diary t)
;; set org-agenda to look at org-roam files too
(setq org-agenda-files `(,org-directory ,org-roam-directory))

;; configure custom face for done state
(add-hook! 'doom-load-theme-hook
           ;; copied pattern for ineriting from .emacs.d/modules/lang/org/config.el
           (custom-declare-face '+org-todo-done '((t (:inherit (bold success org-todo) :strike-through t))) "")
           (custom-declare-face '+org-todo-next `((t (
                                                      :weight bold
                                                      :foreground ,(doom-color 'dark-blue)
                                                      )
                                                     )) "")
           (custom-declare-face '+org-todo-xdone `((t (
                                                      :weight bold
                                                      :foreground ,(doom-color 'grey)
                                                      )
                                                     )) "")
           )
;; configure TODO states
;; based on http://doc.norang.ca/org-mode.html
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

  (setq org-todo-keyword-faces
        '(("TODO"      . +org-todo-active)
          ("NEXT"      . +org-todo-next)
          ("DONE"      . +org-todo-done)
          ("WAITING"   . +org-todo-onhold)
          ("HOLD"      . +org-todo-onhold)
          ("CANCELLED" . +org-todo-xdone)
          ("PHONE"     . +org-todo-xdone)
          ("MEETING"   . +org-todo-xdone)))

  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; configure calendar
(setq calendar-latitude 37.8
      calendar-longitude -122.4
      calendar-location-name "San Francisco, CA")

;; use same directory for deft as org, for convenience
(setq deft-directory org-directory)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
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

;; a few default tweaks
(setq-default
 ;; REVIEW turn off resizing behavior bc I think it might be causing issues
 ;; with SPC w o
 ;; window-combination-resize t ; take space from all windows when splitting
 x-stretch-cursor t          ; take up entire space of glyph
 )
(setq
 auto-save-default t          ; auto save!
 truncate-string-ellipsis "…" ; lets use the real ellipsis character, looks nicer
 )

;; lets make which key show up quick!
(setq which-key-idle-delay 0.5)

;; let avy search across all windows
(setq avy-all-windows t)

;; center text when visual wrapping is on
(setq visual-fill-column-center-text t)

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

;; vlf helps load REALLY large files. it will prompt.
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; improve info colors
(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)

;; made spelling and grammar opt-in
(remove-hook! '(org-mode-hook markdown-mode-hook git-commit-mode-hook)
  #'flyspell-mode)
(remove-hook! '(org-mode-hook markdown-mode-hook)
  #'writegood-mode)
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
