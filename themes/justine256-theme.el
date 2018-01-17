;;; justine256-theme.el --- A theme designed for black xterm-256color.

;; Copyright (C) 2013-2018 Justine Tunney

;; Author: Justine Tunney <jtunney@gmail.com>
;; URL: http://github.com/jart/justinemacs
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This theme is based off tango-dark-theme.el which comes with Emacs.  The
;; colors in this theme come from the Tango palette, which is in the public
;; domain: http://tango.freedesktop.org/

;;; Code:

(deftheme justine256
  "Face colors using the justine256 theme (dark background).
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#555753") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (cham-0 "#b4fa70") (blue-0 "#8cc4ff") (plum-0 "#e6a8df")
      (red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526")
      (diff-green "#00af00") (diff-red "#d70000"))

  (custom-theme-set-faces
   'justine256

   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((((class color) (min-colors 4096))
               (:foreground ,alum-1 :background ,alum-6))
              (((class color) (type tty) (min-colors 256))
               (:foreground ,alum-1 :background nil))
              (((class color) (min-colors 256))
               (:foreground ,alum-1 :background "#222"))
              (,class
               (:foreground ,alum-1 :background "black"))))
   `(cursor ((,class (:background ,butter-1))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,alum-7))))
   `(shadow ((,class (:foreground ,alum-2))))
   `(match ((,class (:foreground ,alum-6 :background ,butter-2))))
   `(highlight ((,class (:foreground ,alum-6 :background ,butter-2))))
   `(hi-yellow ((,class (:background ,plum-1))))
   `(region ((,class (:background ,alum-5))))
   `(secondary-selection ((,class (:background ,blue-3))))
   `(isearch ((,class (:foreground ,alum-1 :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,choc-3))))
   `(trailing-whitespace ((,class (:background ,red-3))))

   ;; Mode line faces
   `(mode-line ((,class
                 (:box (:line-width -1 :style released-button)
                       :background ,alum-2 :foreground "gray10"))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground "gray10"))))
   `(mode-line-inactive ((,class
                          (:box (:line-width -1 :style released-button)
                                :background ,alum-5 :foreground "gray70"))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,cham-0))))
   `(escape-glyph ((,class (:foreground ,butter-3))))
   `(error ((,class (:foreground ,red-0))))
   `(warning ((,class (:foreground ,red-3))))
   `(success ((,class (:foreground ,cham-1))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,plum-1))))
   `(font-lock-comment-face ((,class (:foreground ,cham-2))))
   `(font-lock-constant-face ((,class (:foreground ,plum-0))))
   `(font-lock-function-name-face ((,class (:foreground ,butter-1))))
   `(font-lock-keyword-face ((,class (:foreground ,cham-0))))
   `(font-lock-string-face ((,class (:foreground "#d7af87"))))
   `(font-lock-type-face ((,class (:foreground ,blue-0))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange-1))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-1))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))

   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-news-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-news-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-news-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-news-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-news-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-news-4 ((,class (:foreground ,plum-0))))
   `(gnus-group-news-4-low ((,class (:foreground ,choc-2))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,butter-2))))
   `(gnus-group-mail-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-mail-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-mail-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-mail-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-mail-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-mail-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-mail-low ((,class (:foreground ,butter-2))))
   `(gnus-header-content ((,class (:weight normal :foreground ,butter-3))))
   `(gnus-header-from ((,class (:foreground ,butter-2))))
   `(gnus-header-subject ((,class (:foreground ,cham-1))))
   `(gnus-header-name ((,class (:foreground ,blue-1))))
   `(gnus-header-newsgroups ((,class (:foreground ,choc-2))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue-1))))
   `(message-header-cc ((,class (:foreground ,butter-3))))
   `(message-header-other ((,class (:foreground ,choc-2))))
   `(message-header-subject ((,class (:foreground ,cham-1))))
   `(message-header-to ((,class (:foreground ,butter-2))))
   `(message-cited-text ((,class (:foreground ,cham-1))))
   `(message-separator ((,class (:foreground ,plum-1))))

   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,blue-3))))
   `(smerge-mine ((,class (:background "#eecccc" :foreground "#aa2222"))))
   `(smerge-markers ((,class (:background "grey80" :foreground "grey30"))))
   `(smerge-other ((,class (:background "#cceecc" :foreground "#22aa22"))))

   ;; Ediff faces
   `(ediff-current-diff-A ((,class (:background ,alum-5))))
   `(ediff-fine-diff-A ((,class (:background ,blue-3))))
   `(ediff-even-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-odd-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-current-diff-B ((,class (:background ,alum-5))))
   `(ediff-fine-diff-B ((,class (:background ,choc-3))))
   `(ediff-even-diff-B ((,class (:background ,alum-5.5))))
   `(ediff-odd-diff-B ((,class (:background ,alum-5.5))))

   ;; Diff faces
   `(diff-header ((,class (:foreground ,alum-1 :background "grey20"))))
   `(diff-file-header ((,class (:weight bold :background "grey20"))))
   `(diff-hunk-header ((,class (:inherit diff-header))))
   `(diff-header ((,class (:weight bold :background "grey20"))))
   `(diff-added ((,class (:inherit diff-changed :background "dark green"))))
   `(diff-changed ((,class (:background "midnight blue"))))
   `(diff-indicator-added ((,class (:inherit diff-indicator-changed))))
   `(diff-indicator-changed ((,class (:weight bold))))
   `(diff-indicator-removed ((,class (:inherit diff-indicator-changed))))
   `(diff-removed ((,class (:inherit diff-changed :background "dark red"))))
   `(diff-context ((,class (:foreground ,alum-4))))

   ;; Magit faces
   `(magit-header ((,class (:weight bold :foreground ,alum-1))))
   `(magit-diff-add ((,class (:foreground ,cham-2))))
   `(magit-diff-del ((,class (:foreground "#FCC"))))
   `(magit-diff-file-header ((,class (:inherit magit-header))))
   `(magit-diff-hunk-header ((,class (:inherit magit-header))))
   `(magit-item-highlight ((,class (:background ,alum-7))))
   `(magit-log-head-label ((,class (:background "color-34"))))
   `(magit-log-tag-label ((,class (:background "color-172"))))
   `(magit-section-highlight ((,class (:background ,alum-7))))

   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:foreground ,orange-1))))
   `(flyspell-incorrect ((,class (:foreground ,red-1))))

   ;; whitespace-mode
   `(whitespace-space ((t (:foreground "#333" :background nil))))
   `(whitespace-tab ((t (:foreground "#666" :background nil))))
   `(whitespace-newline ((t (:foreground "#333" :background nil))))
   `(whitespace-line ((t (:background ,alum-5.5 :foreground ,alum-3))))
   `(column-marker-1 ((t (:background ,alum-5.5 :foreground ,alum-3))))

   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline ,alum-4))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,plum-3))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,choc-3))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,red-3))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:background ,alum-5.5))))
   `(semantic-tag-boundary-face ((,class (:overline ,blue-1))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))

   ;; web-mode
   `(web-mode-block-comment-face ((t (:foreground ,cham-2))))
   `(web-mode-block-attr-name-face ((t (:foreground ,plum-1))))
   `(web-mode-block-delimiter-face ((t (:foreground ,alum-4))))
   `(web-mode-block-face ((t (:background "#3a3a3a"))))
   `(web-mode-comment-face ((t (:foreground ,cham-2))))
   `(web-mode-function-call-face ((t (:foreground ,plum-1))))
   `(web-mode-html-attr-name-face ((t (:foreground ,plum-1))))
   `(web-mode-html-attr-equal-face ((t (:foreground ,plum-1))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,alum-4))))
   `(web-mode-html-tag-face ((t (:foreground ,orange-1))))
   `(web-mode-inlay-face ((t (:background "color-237"))))
   `(web-mode-keyword-face ((t (:foreground ,cham-0))))
   `(web-mode-symbol-face ((t (:foreground ,plum-1))))
   `(web-mode-variable-name-face ((t (:foreground ,butter-1))))
   `(web-mode-block-control-face ((t (:foreground ,choc-1))))
   `(web-mode-constant-face ((t (:foreground ,blue-1))))
   `(web-mode-doctype-face ((t (:foreground ,alum-3))))

   ;; JavaScript
   `(js2-warning ((,class (:inherit warning :underline nil))))
   `(js2-error ((,class (:inherit warning :underline nil))))
   `(js2-external-variable ((,class (:foreground ,red-0))))

   ;; FlyCheck
   `(flycheck-error ((,class (:inherit error :underline nil))))
   `(flycheck-info ((,class (:inherit info))))
   `(flycheck-warning ((,class (:inherit warning :underline nil))))

   ;; Gutter
   `(gutter ((,class (:background ,alum-7 :weight bold))))
   `(git-gutter:added ((,class (:inherit gutter :foreground ,diff-green))))
   `(git-gutter:deleted ((,class (:inherit gutter :foreground ,diff-red))))
   `(git-gutter:modified ((,class (:inherit gutter :foreground "magenta"))))
   `(git-gutter:separator ((,class (:inherit gutter :foreground "cyan"))))
   `(git-gutter:unchanged ((,class (:inherit gutter))))

   ;; Customize
   `(custom-variable-tag ((,class (:foreground "color-63" :weight bold))))

   ;; Skewer
   `(skewer-error-face ((,class (:inherit error))))
   `(mmm-default-submode-face ((,class nil))))
  (custom-theme-set-variables
   'justine256
   `(ansi-color-names-vector [,alum-7 ,red-0 ,cham-0 ,butter-1
                                      ,blue-1 ,plum-1 ,blue-0 ,alum-1])))

(provide-theme 'justine256)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; justine256-theme.el ends here
