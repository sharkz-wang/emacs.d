
(deftheme gray-on-black
  "gray-on-black theme")

(let* (;; Variable pitch
       (monokai-pitch (if monokai-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (monokai-class '((class color) (min-colors 257)))

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

       ;; Distinct fringe
       (monokai-fringe-bg (if monokai-distinct-fringe-background
                              monokai-gray
                            monokai-background))

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

       ;; Distinct fringe
       (monokai-256-fringe-bg (if monokai-distinct-fringe-background
                                  monokai-256-gray
                                monokai-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'gray-on-black

   ;; org-mode
   `(org-agenda-structure
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,monokai-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,monokai-background)))
      (,monokai-256-class (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,monokai-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,monokai-256-background)))))

   `(org-agenda-calendar-event
     ((,monokai-class (:foreground ,monokai-emphasis))
      (,monokai-256-class (:foreground ,monokai-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,monokai-class (:foreground ,monokai-foreground
                                   :slant italic))
      (,monokai-256-class (:foreground ,monokai-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,monokai-background)))
      (,monokai-256-class (:foreground ,monokai-256-comments
                                        :background ,monokai-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,monokai-256-background)))) t)

   `(org-agenda-date-weekend
     ((,monokai-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,monokai-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,monokai-256-class (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,monokai-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,monokai-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,monokai-blue
                                :background ,monokai-background))
      (,monokai-256-class (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,monokai-256-blue
                                     :background ,monokai-256-background))) t)

   `(org-agenda-done
     ((,monokai-class (:foreground ,monokai-comments
                                   :slant italic))
      (,monokai-256-class (:foreground ,monokai-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,monokai-class (:foreground ,monokai-comments
                                   :weight normal))
      (,monokai-256-class (:foreground ,monokai-256-comments
                                        :weight normal))))

   `(org-block
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-highlight-alt
				   :extend t))
      (,monokai-256-class (:foreground ,monokai-256-emphasis
                                        :background ,monokai-256-highlight-alt))))

   `(org-block-background
     ((,monokai-class (:background ,monokai-highlight-alt))
      (,monokai-256-class (:background ,monokai-256-highlight-alt))))

   `(org-block-begin-line
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-gray-d
				   :height 0.7
                                   :slant italic
				   :extend t))
      (,monokai-256-class (:foreground ,monokai-256-comments
                                        :background ,monokai-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-gray-d
				   :height 0.7
				   :extend t
                                   :slant italic))
      (,monokai-256-class (:foreground ,monokai-256-comments
                                        :background ,monokai-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground
                                   :box (:line-width 1 :style released-button)))
      (,monokai-256-class (:background ,monokai-256-background
                                        :foreground ,monokai-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class (:foreground ,monokai-256-comments))))

   `(org-date
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))
      (,monokai-256-class (:foreground ,monokai-256-blue
                                        :underline t))))

   `(org-done
     ((,monokai-class (:weight bold :foreground ,monokai-comments))
      (,monokai-256-class (:weight bold
                                    :foreground ,monokai-256-green))))

   `(org-ellipsis
     ((,monokai-class (:foreground ,monokai-comments :underline nil))
      (,monokai-256-class (:foreground ,monokai-256-comments))))

   `(org-formula
     ((,monokai-class (:foreground ,monokai-yellow))
      (,monokai-256-class (:foreground ,monokai-256-yellow))))

   `(org-headline-done
     ((,monokai-class (:weight bold :foreground ,monokai-comments))
      (,monokai-256-class (:foreground ,monokai-256-green))))

   `(org-hide
     ((,monokai-class (:foreground ,monokai-background))
      (,monokai-256-class (:foreground ,monokai-256-background))))

   `(default
      ((,monokai-class (:inherit ,monokai-pitch
				:foreground "#E0E1E0"))
       (,monokai-256-class (:foreground ,monokai-256-foreground
                                         :background ,monokai-256-background))))

   `(org-level-1
     ((,monokai-class (:inherit ,monokai-pitch
                                :height 1.6
				:weight bold
				:extend t
				:underline t
				:foreground "#60D060"))
      (,monokai-256-class (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-4
                                     :foreground ,monokai-256-orange))))

   `(org-level-2
     ((,monokai-class (:inherit ,monokai-pitch
                                :height 1.3
				:weight bold
				:foreground "#D2D4D9"))
      (,monokai-256-class (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-3
                                     :foreground ,monokai-256-green))))

   `(org-level-3
     ((,monokai-class (:inherit ,monokai-pitch
				:weight bold
				:foreground  "#d4d7d9"))
      (,monokai-256-class (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-2
                                     :foreground ,monokai-256-blue))))

   `(org-level-4
     ((,monokai-class (:inherit ,monokai-pitch
                                :height ,monokai-height-plus-1
                                :foreground "#bebfb3"))
      (,monokai-256-class (:inherit ,monokai-pitch
                                     :height ,monokai-height-plus-1
                                     :foreground ,monokai-256-yellow))))

   `(org-level-5
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-cyan))
      (,monokai-256-class (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-cyan))))

   `(org-level-6
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-green))
      (,monokai-256-class (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-green))))

   `(org-level-7
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-red))
      (,monokai-256-class (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-red))))

   `(org-level-8
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-blue))
      (,monokai-256-class (:inherit ,monokai-pitch
                                     :foreground ,monokai-256-blue))))

   `(org-link
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))
      (,monokai-256-class (:foreground ,monokai-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,monokai-class (:foreground ,monokai-violet))
      (,monokai-256-class (:foreground ,monokai-256-violet))))

   `(org-scheduled
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class (:foreground ,monokai-256-green))))

   `(org-scheduled-previously
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class (:foreground ,monokai-256-cyan))))

   `(org-scheduled-today
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight normal))
      (,monokai-256-class (:foreground ,monokai-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,monokai-class (:foreground ,monokai-comments
                                   :weight bold))
      (,monokai-256-class (:foreground ,monokai-256-comments
                                        :weight bold))))

   `(org-table
     ((,monokai-class (:foreground ,monokai-green))
      (,monokai-256-class (:foreground ,monokai-256-green))))

   `(org-tag
     ((,monokai-class (:weight bold))
      (,monokai-256-class (:weight bold))))

   `(org-time-grid
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class (:foreground ,monokai-256-comments))))

   `(org-todo
     ((,monokai-class (:foreground "#23DBD5"
                                   :weight bold)))
     ((,monokai-256-class (:foreground ,monokai-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal
                                   :underline nil))
      (,monokai-256-class (:foreground ,monokai-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight normal
                                   :underline nil))
      (,monokai-256-class (:foreground ,monokai-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,monokai-class (:background ,monokai-blue-lc
                                   :foreground ,monokai-blue-hc))
      (,monokai-256-class (:background ,monokai-256-blue-lc
                                        :foreground ,monokai-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,monokai-class (:background ,monokai-blue-lc))
      (,monokai-256-class (:background ,monokai-256-blue-lc))))

   `(org-habit-ready-face
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-green))
      (,monokai-256-class (:background ,monokai-256-green-lc
                                        :foreground ,monokai-256-green))))

   `(org-habit-ready-future-face
     ((,monokai-class (:background ,monokai-green-lc))
      (,monokai-256-class (:background ,monokai-256-green-lc))))

   `(org-habit-alert-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-yellow-lc))
      (,monokai-256-class (:background ,monokai-256-yellow
                                        :foreground ,monokai-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,monokai-class (:background ,monokai-yellow-lc))
      (,monokai-256-class (:background ,monokai-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-red-lc))
      (,monokai-256-class (:background ,monokai-256-red
                                        :foreground ,monokai-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,monokai-class (:background ,monokai-red-lc))
      (,monokai-256-class (:background ,monokai-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,monokai-class (:foreground ,monokai-comments))
      (,monokai-256-class (:foreground ,monokai-256-comments))))

   `(org-agenda-restriction-lock
     ((,monokai-class (:background ,monokai-yellow))
      (,monokai-256-class (:background ,monokai-256-yellow))))

   `(org-clock-overlay
     ((,monokai-class (:background ,monokai-yellow))
      (,monokai-256-class (:background ,monokai-256-yellow))))

   `(org-column
     ((,monokai-class (:background ,monokai-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,monokai-256-class (:background ,monokai-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,monokai-class (:background ,monokai-highlight-line
                                   :underline t
                                   :weight bold))
      (,monokai-256-class (:background ,monokai-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,monokai-class (:foreground ,monokai-red
                                   :inverse-video t))
      (,monokai-256-class (:foreground ,monokai-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,monokai-class (:foreground ,monokai-foreground))
      (,monokai-256-class (:foreground ,monokai-256-foreground))))

   `(org-document-title
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :weight bold
                                   :height ,monokai-height-plus-4))
      (,monokai-256-class (:foreground ,monokai-256-emphasis
                                        :weight bold
                                        :height ,monokai-height-plus-4))))

   `(org-drawer
     ((,monokai-class (:foreground ,monokai-cyan))
      (,monokai-256-class (:foreground ,monokai-256-cyan))))

   `(org-footnote
     ((,monokai-class (:foreground ,monokai-magenta
                                   :underline t))
      (,monokai-256-class (:foreground ,monokai-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,monokai-class (:foreground ,monokai-orange))
      (,monokai-256-class (:foreground ,monokai-256-orange))))

   `(org-mode-line-clock-overrun
     ((,monokai-class (:inherit mode-line))
      (,monokai-256-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,monokai-class (:inherit org-level-1))
      (,monokai-256-class (:inherit org-level-1))))

   `(outline-2
     ((,monokai-class (:inherit org-level-2))
      (,monokai-256-class (:inherit org-level-2))))

   `(outline-3
     ((,monokai-class (:inherit org-level-3))
      (,monokai-256-class (:inherit org-level-3))))

   `(outline-4
     ((,monokai-class (:inherit org-level-4))
      (,monokai-256-class (:inherit org-level-4))))

   `(outline-5
     ((,monokai-class (:inherit org-level-5))
      (,monokai-256-class (:inherit org-level-5))))

   `(outline-6
     ((,monokai-class (:inherit org-level-6))
      (,monokai-256-class (:inherit org-level-6))))

   `(outline-7
     ((,monokai-class (:inherit org-level-7))
      (,monokai-256-class (:inherit org-level-7))))

   `(outline-8
     ((,monokai-class (:inherit org-level-8))
      (,monokai-256-class (:inherit org-level-8))))
   )
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gray-on-black)
