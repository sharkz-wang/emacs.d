;;; black-on-gray-theme.el --- black-on-gray theme

;; Copyright (C) 2002 by sbhojwani
;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA &lt;syohex@gmail.com&gt;
;; URL: https://github.com/emacs-jp/replace-colorthemes
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see &lt;http://www.gnu.org/licenses/&gt;.

;;; Commentary:
;;
;; Port of black-on-gray theme from `color-themes'

;;; Code:

(deftheme black-on-gray
  "black-on-gray theme")

(let*
    ;; color palette of monokai theme
    (
     ;; Primary colors
     (monokai-yellow "#E6DB74")
     (monokai-orange "#FD971F")
     (monokai-red "#F92672")
     (monokai-magenta "#FD5FF0")
     (monokai-blue "#66D9EF")
     (monokai-green "#A6E22E")
     (monokai-cyan "#A1EFE4")
     (monokai-violet "#AE81FF")
     (monokai-gray "#64645E")
     (monokai-foreground "#F8F8F2")
     (monokai-background "#b0b0b0")
     (monokai-comments "#75715E")
     (monokai-emphasis "#F8F8F0")
     (monokai-line-number "#8F908A")
     (monokai-highlight "#49483E")
     (monokai-highlight-alt "#3E3D31")
     (monokai-highlight-line "#3C3D37")

     ;; Functionality specific colors
     (monokai-diff-blue-base      "#232438")
     (monokai-diff-blue-emphasis  "#1F204E")
     (monokai-diff-green-base     "#233E1E")
     (monokai-diff-green-emphasis "#1F541A")
     (monokai-diff-red-base       "#3D241E")
     (monokai-diff-red-emphasis   "#53201A")

     ;; Darker and lighter accented colors
     (monokai-yellow-d       "#BEB244")
     (monokai-yellow-l       "#FFF7A8")
     (monokai-orange-d       "#D47402")
     (monokai-orange-l       "#FFAC4A")
     (monokai-red-d          "#F70057")
     (monokai-red-l          "#FA518D")
     (monokai-magenta-d      "#FB35EA")
     (monokai-magenta-l      "#FE8CF4")
     (monokai-violet-d       "#945AFF")
     (monokai-violet-l       "#C9ACFF")
     (monokai-blue-d         "#40CAE4")
     (monokai-blue-l         "#92E7F7")
     (monokai-cyan-d         "#74DBCD")
     (monokai-cyan-l         "#D3FBF6")
     (monokai-green-d        "#86C30D")
     (monokai-green-l        "#BBEF53")
     (monokai-gray-d         "#35331D")
     (monokai-gray-l         "#7B7962")
     ;; Adaptive higher/lower contrast accented colors
     (monokai-foreground-hc  "#141414")
     (monokai-foreground-lc  "#171A0B")
     ;; High contrast colors
     (monokai-yellow-hc      "#FFFACE")
     (monokai-yellow-lc      "#9A8F21")
     (monokai-orange-hc      "#FFBE74")
     (monokai-orange-lc      "#A75B00")
     (monokai-red-hc         "#FEB0CC")
     (monokai-red-lc         "#F20055")
     (monokai-magenta-hc     "#FEC6F9")
     (monokai-magenta-lc     "#F309DF")
     (monokai-violet-hc      "#F0E7FF")
     (monokai-violet-lc      "#7830FC")
     (monokai-blue-hc        "#CAF5FD")
     (monokai-blue-lc        "#1DB4D0")
     (monokai-cyan-hc        "#D3FBF6")
     (monokai-cyan-lc        "#4BBEAE")
     (monokai-green-hc       "#CCF47C")
     (monokai-green-lc       "#679A01")

     ;; Definitions for terminals that do not support 256 colors
     (monokai-256-class '((class color) (min-colors 89)))

     ;; Functionality specific colors
     (monokai-256-diff-blue-base      "#00005f")
     (monokai-256-diff-blue-emphasis  "#000087")
     (monokai-256-diff-green-base     "#005800")
     (monokai-256-diff-green-emphasis "#008700")
     (monokai-256-diff-red-base       "#5f0000")
     (monokai-256-diff-red-emphasis   "#870000")

     ;; Primary colors
     (monokai-256-yellow         "#CDC673")
     (monokai-256-orange         "#FF8C00")
     (monokai-256-red            "#FF1493")
     (monokai-256-magenta        "#D700D7")
     (monokai-256-violet         "#AF87FF")
     (monokai-256-blue           "#5FD7FF")
     (monokai-256-cyan           "#5FFFFF")
     (monokai-256-green          "#87D700")
     (monokai-256-gray           "#3D3D3D")
     ;; Darker and lighter accented colors
     (monokai-256-yellow-d       "#878700")
     (monokai-256-yellow-l       "#FFFF87")
     (monokai-256-orange-d       "#AF5F00")
     (monokai-256-orange-l       "#FFAF5F")
     (monokai-256-red-d          "#870000")
     (monokai-256-red-l          "#FF5F87")
     (monokai-256-magenta-d      "#AF0087")
     (monokai-256-magenta-l      "#FF87DF")
     (monokai-256-violet-d       "#5F00AF")
     (monokai-256-violet-l       "#AF87D7")
     (monokai-256-blue-d         "#008787")
     (monokai-256-blue-l         "#87D7FF")
     (monokai-256-cyan-d         "#5FAFAF")
     (monokai-256-cyan-l         "#AFFFFF")
     (monokai-256-green-d        "#5F8700")
     (monokai-256-green-l        "#AFD700")
     (monokai-256-gray-d         "#333333")
     (monokai-256-gray-l         "#707070")
     ;; Adaptive colors
     (monokai-256-foreground     "#F5F5F5")
     (monokai-256-background     "#1B1E1C")
     (monokai-256-comments       "#8B8878")
     (monokai-256-emphasis       "#FFFAFA")
     (monokai-256-line-number    "#8F908A")
     (monokai-256-highlight      "#474747")
     (monokai-256-highlight-alt  "#3E3E3E")
     (monokai-256-highlight-line "#000000")
     ;; Adaptive higher/lower contrast accented colors
     (monokai-256-foreground-hc  "#171A0B")
     (monokai-256-foreground-lc  "#141414")
     ;; High contrast colors
     (monokai-256-yellow-hc      monokai-256-yellow-d)
     (monokai-256-yellow-lc      monokai-256-yellow-l)
     (monokai-256-orange-hc      monokai-256-orange-d)
     (monokai-256-orange-lc      monokai-256-orange-l)
     (monokai-256-red-hc         monokai-256-red-d)
     (monokai-256-red-lc         monokai-256-red-l)
     (monokai-256-magenta-hc     monokai-256-magenta-d)
     (monokai-256-magenta-lc     monokai-256-magenta-l)
     (monokai-256-violet-hc      monokai-256-violet-d)
     (monokai-256-violet-lc      monokai-256-violet-l)
     (monokai-256-blue-hc        monokai-256-blue-d)
     (monokai-256-blue-lc        monokai-256-blue-l)
     (monokai-256-cyan-hc        monokai-256-cyan-d)
     (monokai-256-cyan-lc        monokai-256-cyan-l)
     (monokai-256-green-hc       monokai-256-green-d)
     (monokai-256-green-lc       monokai-256-green-l)
     )

  (custom-theme-set-faces
   'black-on-gray

   '(default ((t (:background "#FAFAFA" :foreground "black"))))
   '(border ((t (:foregound "blue"))))

   '(blue ((t (:foreground "blue"))))
   '(bold ((t (:bold t :size "10pt"))))
   '(bold-italic ((t (:italic t :bold t :size "10pt"))))
   '(border-glyph ((t (:size "11pt"))))
   '(buffers-tab ((t (:background "gray75"))))
   '(buffers-tab-face ((t (:background "gray75"))))
   '(display-time-mail-balloon-enhance-face ((t (:background "orange"))))
   '(display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))
   '(display-time-time-balloon-face ((t (:foreground "red"))))
   '(ecb-bucket-token-face ((t (:bold t :size "10pt"))))
   '(ecb-default-general-face ((t (nil))))
   '(ecb-default-highlight-face ((t (:background "cornflower blue" :foreground "yellow"))))
   '(ecb-directories-general-face ((t (nil))))
   '(ecb-directory-face ((t (:background "cornflower blue" :foreground "yellow"))))
   '(ecb-history-face ((t (:background "cornflower blue" :foreground "yellow"))))
   '(ecb-history-general-face ((t (nil))))
   '(ecb-method-face ((t (:background "cornflower blue" :foreground "yellow"))))
   '(ecb-methods-general-face ((t (nil))))
   '(ecb-source-face ((t (:background "cornflower blue" :foreground "yellow"))))
   '(ecb-source-in-directories-buffer-face ((t (:foreground "medium blue"))))
   '(ecb-sources-general-face ((t (nil))))
   '(ecb-token-header-face ((t (:background "SeaGreen1"))))
   '(ecb-type-token-class-face ((t (:bold t :size "10pt"))))
   '(ecb-type-token-enum-face ((t (:bold t :size "10pt"))))
   '(ecb-type-token-group-face ((t (:bold t :size "10pt" :foreground "dimgray"))))
   '(ecb-type-token-interface-face ((t (:bold t :size "10pt"))))
   '(ecb-type-token-struct-face ((t (:bold t :size "10pt"))))
   '(ecb-type-token-typedef-face ((t (:bold t :size "10pt"))))
   `(font-lock-builtin-face ((t (:foreground ,monokai-red :weight normal))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,monokai-comments))))
   `(font-lock-comment-face ((t (:foreground ,monokai-comments))))
   `(font-lock-constant-face ((t (:foreground ,monokai-violet))))
   `(font-lock-doc-face ((t (:foreground ,monokai-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,monokai-green))))
   `(font-lock-keyword-face ((t (:foreground ,monokai-red :weight normal))))
   `(font-lock-negation-char-face ((t (:foreground ,monokai-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,monokai-red))))
   `(font-lock-regexp-grouping-construct
     ((t (:foreground ,monokai-yellow :weight normal))))
   `(font-lock-regexp-grouping-backslash
     ((t (:foreground ,monokai-violet :weight normal))))
   `(font-lock-string-face ((t (:foreground ,monokai-yellow))))
   `(font-lock-type-face ((t (:foreground ,monokai-blue :italic nil))))
   `(font-lock-variable-name-face ((t (:foreground ,monokai-orange))))
   `(font-lock-warning-face
     ((t (:foreground ,monokai-orange :weight bold :italic t :underline t))))
   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
   '(green ((t (:foreground "green"))))
   '(gui-button-face ((t (:background "grey75"))))
   '(gui-element ((t (:size "8pt" :background "gray75"))))
   '(highlight ((t (:background "gray95"))))
   '(isearch ((t (:background "paleturquoise"))))
   '(isearch-secondary ((t (:foreground "red3"))))
   '(italic ((t (:size "10pt"))))
   '(left-margin ((t (nil))))
   '(list-mode-item-selected ((t (:background "gray68"))))
   '(modeline ((t (:background "gray75"))))
   '(modeline-buffer-id ((t (:background "gray75" :foreground "blue4"))))
   '(modeline-mousable ((t (:background "gray75" :foreground "firebrick"))))
   '(modeline-mousable-minor-mode ((t (:background "gray75" :foreground "green4"))))
   '(paren-blink-off ((t (:foreground "gray"))))
   '(paren-match ((t (:background "darkseagreen2"))))
   '(paren-mismatch ((t (nil))))
   '(pointer ((t (nil))))
   '(primary-selection ((t (:background "gray65"))))
   '(red ((t (:foreground "red"))))
   '(region ((t (:background "gray65"))))
   '(right-margin ((t (nil))))
   '(secondary-selection ((t (:background "paleturquoise"))))
   '(semantic-dirty-token-face ((t (nil))))
   '(semantic-unmatched-syntax-face ((t (nil))))
   '(text-cursor ((t (:background "red" :foreground "gray"))))
   '(toolbar ((t (:background "gray75"))))
   '(underline ((t (:underline t))))
   '(vertical-divider ((t (:background "gray75"))))
   '(widget ((t (:size "8pt" :background "gray75"))))
   '(widget-button-face ((t (:bold t))))
   '(widget-button-pressed-face ((t (:foreground "red"))))
   '(widget-documentation-face ((t (:foreground "dark green"))))
   '(widget-field-face ((t (:background "gray85"))))
   '(widget-inactive-face ((t (nil))))
   '(yellow ((t (:foreground "yellow"))))
   '(zmacs-region ((t (:background "gray65"))))

   ;; org-mode

   '(org-meta-line
     ((t (:foreground "gray90"))))
   '(org-block-begin-line
     ((t (:foreground "gray90" :background "gray90"))))
   '(org-block-end-line
     ((t (:foreground "gray90" :background "gray90"))))
   '(org-block ((t (:background "#F5F5F5"))))

   '(org-todo ((t (:height 1.3 :foreground "purple" :weight bold))))
   '(org-level-1 ((t (:height 1.6 :weight bold))))
   '(org-level-2 ((t (:height 1.2 :weight bold))))
   '(org-level-3 ((t (:height 1.1 :weight bold))))
   '(org-level-4 ((t (:height 1.0))))
   '(org-level-5 ((t (:height 1.0))))
   '(org-level-6 ((t (:height 1.0))))
   '(org-level-7 ((t (:height 1.0))))
   '(org-level-8 ((t (:height 1.0))))

   '(org-hide ((t (:foreground "#FAFAFA")))) ;; hide leading bullets
   )
)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'black-on-gray)

;;; black-on-gray-theme.el ends here
