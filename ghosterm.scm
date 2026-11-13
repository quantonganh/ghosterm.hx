(require "helix/editor.scm")
(require "helix/configuration.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require "expansions.hx/expansions.scm")

(#%require-dylib "libghosterm"
                 (only-in default-shell
                          create-pty
                          pty-write
                          pty-write-char
                          pty-resize
                          pty-kill
                          pty-alive?
                          pty-read
                          virtual-terminal
                          vte/advance-bytes
                          vte/resize
                          vte/reset-iterator!
                          vte/advance-iterator!
                          vte/iter-x
                          vte/iter-y
                          vte/iter-cell-str
                          vte/iter-cell-fg
                          vte/iter-cell-bg
                          vte/iter-cell-attrs
                          vte/cursor-x
                          vte/cursor-y
                          vte/cursor-visible?
                          vte/scroll-up
                          vte/scroll-down
                          vte/scroll-top
                          vte/scroll-bottom))

(provide terminal
         terminal-cycle
         terminal-focus
         terminal-run
         terminal-run-config
         terminal-toggle)

(define *terminal-shell* (default-shell))

;; Each split direction owns a group of terminal tabs.
(define *terminals* (hash))
(define *last-direction* 'bottom)
(define *focused-direction* #f)

;; Number of text rows the bottom pane occupies (the top border is extra).
;; #f means "half of the current editor height", recomputed on every render.
(define *terminal-height* #f)

;; Number of text columns the right pane occupies (the left border is extra).
;; #f means "30% of the current editor width", recomputed on every render.
(define *terminal-width* #f)

(define (default-terminal-height)
  (let ([area (editor-focused-buffer-area)])
    (if area
        (max 1 (quotient (area-height area) 2))
        24)))

(define (terminal . args)
  (if (null? args)
    (show-terminal))
    (terminal-exec-in (terminal-exec-command args)))

(define (terminal-toggle) (toggle-terminal))

(define (direction-name direction)
  (if (eq? direction 'right) "right" "bottom"))

(define (component-name direction)
  (string-append "ghosterm-" (direction-name direction)))

(define (parse-direction value)
  (cond
    [(not value) #f]
    [(equal? value "right") 'right]
    [(equal? value "bottom") 'bottom]
    [else #f]))

(define (terminal-get direction)
  (hash-try-get *terminals* direction))

(define (terminal-put! direction term)
  (set! *terminals* (hash-insert *terminals* direction term)))

(define (terminal-remove! direction)
  (set! *terminals* (hash-remove *terminals* direction)))

(define (terminal-default-direction)
  (if (and *focused-direction* (terminal-get *focused-direction*))
      *focused-direction*
      *last-direction*))

(define (direction-argument args fallback)
  (or (and (pair? args) (parse-direction (car args))) fallback))

(define (command-arguments args)
  (if (and (pair? args) (parse-direction (car args)))
      (cdr args)
      args))

(define (set-editor-clip! direction amount)
  (if (eq? direction 'right)
      (set-editor-clip-right! amount)
      (set-editor-clip-bottom! amount)))

(struct TerminalTabs (direction tabs active pushed?))

(define (terminal-tabs-list group)
  (unbox (TerminalTabs-tabs group)))

(define (terminal-tabs-active-index group)
  (unbox (TerminalTabs-active group)))

(define (terminal-tabs-active-term group)
  (define tabs (terminal-tabs-list group))
  (define index (terminal-tabs-active-index group))
  (if (or (null? tabs) (>= index (length tabs)))
      #f
      (list-ref tabs index)))

(define (terminal-active-term direction)
  (define group (terminal-get direction))
  (and group (terminal-tabs-active-term group)))

(define (spawn-terminal-tab direction)
  (define rows
    (if (eq? direction 'right)
        24
        (or *terminal-height* (default-terminal-height))))
  (define term (make-terminal direction *terminal-shell* rows 80))
  (terminal-loop term)
  term)

(define (append-terminal-tab! group term)
  (define tabs (terminal-tabs-list group))
  (set-box! (TerminalTabs-tabs group) (append tabs (list term)))
  (set-box! (TerminalTabs-active group) (length tabs))
  (set-box! (Terminal-active term) #t))

(define (replace-terminal-tab! group index term)
  (set-box! (TerminalTabs-tabs group)
            (replace-list-at (terminal-tabs-list group) index term)))

(define (replace-list-at items index value)
  (cond
    [(null? items) (list value)]
    [(= index 0) (cons value (cdr items))]
    [else (cons (car items)
                (replace-list-at (cdr items) (- index 1) value))]))

(define (remove-list-at items index)
  (cond
    [(null? items) '()]
    [(= index 0) (cdr items)]
    [else (cons (car items) (remove-list-at (cdr items) (- index 1)))]))

(define (new-terminal-tab direction)
  (if (not (terminal-get direction))
      (show-terminal direction)
      (let* ([group (terminal-get direction)]
             [old (terminal-tabs-active-term group)]
             [was-focused? (and old (unbox (Terminal-focused? old)))]
             [term (spawn-terminal-tab direction)])
        (when old
          (set-box! (Terminal-focused? old) #f))
        (append-terminal-tab! group term)
        (set-box! (Terminal-active term) #t)
        (when was-focused?
          (set-box! (Terminal-focused? term) #t))
        (helix.redraw))))

(define (switch-terminal-tab! direction index)
  (define group (terminal-get direction))
  (when group
    (let* ([tabs (terminal-tabs-list group)]
           [count (length tabs)])
      (when (and (>= index 0) (< index count))
        (let* ([old (terminal-tabs-active-term group)]
               [was-focused? (and old (unbox (Terminal-focused? old)))])
          (when old
            (set-box! (Terminal-focused? old) #f))
          (set-box! (TerminalTabs-active group) index)
          (let ([term (terminal-tabs-active-term group)])
            (set-box! (Terminal-active term) #t)
            (when was-focused?
              (set-box! (Terminal-focused? term) #t))
            (helix.redraw)))))))

(define (switch-terminal-tab-relative direction delta)
  (define group (terminal-get direction))
  (when group
    (let ([count (length (terminal-tabs-list group))])
      (when (> count 0)
        (switch-terminal-tab!
         direction
         (modulo (+ (terminal-tabs-active-index group) delta) count))))))

(define (show-terminal direction)
  (define group (terminal-get direction))

  (unless group
    (set! group (TerminalTabs direction (box '()) (box 0) (box #f)))
    (terminal-put! direction group))

  (when (null? (terminal-tabs-list group))
    (append-terminal-tab! group (spawn-terminal-tab direction)))

  (define term (terminal-tabs-active-term group))

  (unless (terminal-loop-alive? term)
    (replace-terminal-tab! group (terminal-tabs-active-index group)
                           (spawn-terminal-tab direction))
    (set! term (terminal-tabs-active-term group)))

  (set-box! (Terminal-active term) #t)
  (focus-terminal! direction)

  (unless (unbox (TerminalTabs-pushed? group))
    (set-box! (TerminalTabs-pushed? group) #t)
    (push-component!
     (new-component! (component-name direction)
                     group
                     terminal-render
                     (component-functions group))))

  (helix.redraw))

(define (component-functions group)
    (define with-handle
        (hash-insert (hash) "handle_event" terminal-event-handler))
    (hash-insert with-handle "cursor" terminal-cursor-handler))

(define (terminal-loop-alive? term)
  (and term (not (unbox (Terminal-kill-switch term)))))

(define (terminal-visible? direction)
  (define term (terminal-active-term direction))
  (and term
       (unbox (Terminal-active term))
       (terminal-loop-alive? term)))

(define (terminal-cycle)
  (cond
    [(not *focused-direction*)
     (cond
       [(terminal-visible? 'bottom)
        (focus-terminal! 'bottom)
        (helix.redraw)]
       [(terminal-visible? 'right)
        (focus-terminal! 'right)
        (helix.redraw)]
       [else (set-status! "No terminal is open")])]
    [(eq? *focused-direction* 'bottom)
     (if (terminal-visible? 'right)
         (begin
           (focus-terminal! 'right)
           (helix.redraw))
         (blur-terminal!))]
    [else
     (blur-terminal!)]))

(define (terminal-focus . args)
  (define direction (direction-argument args (terminal-default-direction)))
  (define term (terminal-active-term direction))
  (cond
    [(and term (unbox (Terminal-focused? term)))
      (blur-terminal!)]
    [(and term
          (unbox (Terminal-active term))
          (terminal-loop-alive? term))
      (focus-terminal! direction)
      (helix.redraw)]
    [else (show-terminal direction)]))

(define (focus-terminal! direction)
  (define term (terminal-active-term direction))
  (when term
    (for-each
      (lambda (other)
        (define other-term (terminal-active-term other))
        (when (and other-term (not (eq? other direction)))
          (set-box! (Terminal-focused? other-term) #f)))
      (list 'bottom 'right))
    (set-box! (Terminal-focused? term) #t)
    (set! *focused-direction* direction)
    (set! *last-direction* direction)
    (auto-save-editor!)
    (save-editor-cursor-shapes!)
    (hide-editor-cursor!)))

(define (blur-terminal!)
  (define direction *focused-direction*)
  (when direction
    (define term (terminal-active-term direction))
    (when term
      (set-box! (Terminal-focused? term) #f))
    (set! *focused-direction* #f)
    (restore-editor-cursor!)
    (auto-reload-editor!)
    (helix.redraw)))

(struct Terminal
        (direction cursor pty vte size focused? active kill-switch area
              cursor-shown?))

(define (make-terminal direction shell rows cols)
  (define pty (create-pty shell rows cols))
  (define vt (virtual-terminal pty rows cols))
  (Terminal direction
            (position 0 0)
            pty
            vt
            (box #f)
            (box #f)
            (box #f)
            (box #f)
            (box #f)
            (box #f)))

(define (terminal-loop term)
  (define pty (Terminal-pty term))
  (define vt (Terminal-vte term))
  (define kill-switch (Terminal-kill-switch term))
  (if (unbox kill-switch)
      void
      (helix-await-callback
       (pty-read pty)
       (lambda (bytes)
         (cond
           [bytes
            (vte/advance-bytes vt bytes)
            (terminal-loop term)]
           [(pty-alive? pty)
            (terminal-loop term)]
            [else
             ;; Keep the final screen visible; the next show command will
             ;; replace this component with a fresh shell.
             (let ([was-focused? (unbox (Terminal-focused? term))])
               (set-box! kill-switch #t)
               (set-box! (Terminal-focused? term) #f)
               (when (and was-focused?
                          (eq? *focused-direction* (Terminal-direction term)))
                 (set! *focused-direction* #f)
                 (restore-editor-cursor!)
                 (auto-reload-editor!))
               (helix.redraw))])))))

(define (terminal-render group rect frame)
  (when group
    (define direction (TerminalTabs-direction group))
    (define state (terminal-tabs-active-term group))
    (if (and state (unbox (Terminal-active state)))
        (if (eq? direction 'right)
            (render-right-terminal group state rect frame)
            (render-bottom-terminal group state rect frame))
        (set-editor-clip! direction 0))))

(define (render-bottom-terminal group state rect frame)
  (define pane (terminal-pane-area rect))

  (cond
    [(not pane)
     ;; Not enough room to reserve a safe editor area.
     (set-editor-clip-bottom! 0)]

    [else
     (define vt (Terminal-vte state))
     (define pty (Terminal-pty state))
     (define focused (unbox (Terminal-focused? state)))

     (define text-rows (max 1 (- (area-height pane) 1)))
     (define cols (max 1 (area-width pane)))

     ;; Reserve the bottom pane.
     (set-editor-clip-bottom! (- (area-height pane) 1))
     (set-box! (Terminal-area state) pane)

     ;; Resize only when dimensions change.
     (define target-size (list text-rows cols))
     (unless (equal? (unbox (Terminal-size state)) target-size)
       (set-box! (Terminal-size state) target-size)
       (vte/resize vt text-rows cols)
       (pty-resize pty text-rows cols))

     (define theme-fg
       (or (style->fg (theme->fg *helix.cx*)) Color/White))
     (define theme-bg
       (or (style->bg (theme->bg *helix.cx*)) Color/Black))
     (define border-fg theme-fg)
     (define border-base (style-fg (theme->bg *helix.cx*) border-fg))
     (define border-style
       (if focused border-base (style-with-dim border-base)))

     (buffer/clear frame pane)
     (block/render frame
                   pane
                   (make-block (theme->bg *helix.cx*)
                               border-style
                               "top"
                               "plain"))
     (render-tab-bar frame pane group focused border-base border-style)

     (define x-offset (area-x pane))
     (define y-offset (+ (area-y pane) 1))

     (vte/reset-iterator! vt)
     (while (vte/advance-iterator! vt)
       (render-current-cell vt frame x-offset y-offset theme-fg theme-bg))

     (define cursor (Terminal-cursor state))
     (define cursor-y (vte/cursor-y vt))
     (define cursor-x (vte/cursor-x vt))
     ;; After scrolling the viewport the terminal cursor can be outside the
     ;; visible rows, in which case its coordinates are negative. Passing
     ;; those through would wrap to 65535 and panic the terminal backend.
     (define cursor-shown?
       (and focused
            (vte/cursor-visible? vt)
             (>= cursor-y 0)
             (< cursor-y text-rows)
             (>= cursor-x 0)
             (< cursor-x cols)))
     (set-box! (Terminal-cursor-shown? state) cursor-shown?)
     (when cursor-shown?
       (set-position-row! cursor (+ y-offset cursor-y))
       (set-position-col! cursor (+ x-offset cursor-x)))]))

(define (tab-label index)
  (string-append " " (number->string (+ index 1)) " "))

(define (render-tab-bar frame pane group focused border-base border-style)
  (define x (+ (area-x pane) 1))
  (define max-x (+ (area-x pane) (area-width pane)))
  (let loop ([tabs (terminal-tabs-list group)]
             [i 0])
    (when (pair? tabs)
      (let* ([label (tab-label i)]
             [style
              (cond
                [(= i (terminal-tabs-active-index group))
                 (if focused (style-with-bold border-base) border-style)]
                [else
                 (if focused (style-with-dim border-base) border-style)])])
        (when (< (+ x (string-length label)) max-x)
          (frame-set-string! frame x (area-y pane) label style)
          (set! x (+ x (string-length label) 1))))
      (loop (cdr tabs) (+ i 1)))))

(define (tab-index-at-column group pane col)
  (define x (+ (area-x pane) 1))
  (define max-x (+ (area-x pane) (area-width pane)))
  (let loop ([tabs (terminal-tabs-list group)]
             [i 0])
    (cond
      [(null? tabs) #f]
      [else
       (let* ([label (tab-label i)]
              [width (string-length label)])
         (if (and (>= col x)
                  (< col (+ x width))
                  (< (+ x width) max-x))
             i
             (begin
               (set! x (+ x width 1))
               (loop (cdr tabs) (+ i 1)))))])))

(define (render-right-terminal _group state rect frame)
  (define pane (terminal-pane-area-right rect))

  (cond
    [(not pane)
     (set-editor-clip-right! 0)]

    [else
     (define vt (Terminal-vte state))
     (define pty (Terminal-pty state))
     (define focused (unbox (Terminal-focused? state)))

     (define text-cols (max 1 (- (area-width pane) 1)))
     (define text-rows (max 1 (area-height pane)))

     (set-editor-clip-right! (- (area-width pane) 1))
     (set-box! (Terminal-area state) pane)

     (define target-size (list text-rows text-cols))
     (unless (equal? (unbox (Terminal-size state)) target-size)
       (set-box! (Terminal-size state) target-size)
       (vte/resize vt text-rows text-cols)
       (pty-resize pty text-rows text-cols))

     (define theme-fg
       (or (style->fg (theme->fg *helix.cx*)) Color/White))
     (define theme-bg
       (or (style->bg (theme->bg *helix.cx*)) Color/Black))
     (define border-base (style-fg (theme->bg *helix.cx*) theme-fg))
     (define border-style
       (if focused border-base (style-with-dim border-base)))

     (buffer/clear frame pane)
     (block/render frame
                   pane
                   (make-block (theme->bg *helix.cx*)
                               border-style
                               "left"
                               "plain"))

     (define x-offset (+ (area-x pane) 1))
     (define y-offset (area-y pane))

     (vte/reset-iterator! vt)
     (while (vte/advance-iterator! vt)
       (render-current-cell vt frame x-offset y-offset theme-fg theme-bg))

     (define cursor (Terminal-cursor state))
     (define cursor-y (vte/cursor-y vt))
     (define cursor-x (vte/cursor-x vt))
     (define cursor-shown?
       (and focused
            (vte/cursor-visible? vt)
            (>= cursor-y 0)
            (< cursor-y text-rows)
            (>= cursor-x 0)
            (< cursor-x text-cols)))
     (set-box! (Terminal-cursor-shown? state) cursor-shown?)
     (when cursor-shown?
       (set-position-row! cursor (+ y-offset cursor-y))
       (set-position-col! cursor (+ x-offset cursor-x)))]))

(define (render-current-cell vt frame x-offset y-offset theme-fg theme-bg)
  (define text (vte/iter-cell-str vt))
  (unless (string=? text "")
    (frame-set-string! frame
                       (+ x-offset (vte/iter-x vt))
                       (+ y-offset (vte/iter-y vt))
                       text
                       (cell-style (vte/iter-cell-fg vt)
                                   (vte/iter-cell-bg vt)
                                   (vte/iter-cell-attrs vt)
                                   theme-fg
                                   theme-bg))))

(define (cell-style fg bg attrs theme-fg theme-bg)
  (define inverse? (ghost-bit? attrs 4))
  (define actual-fg (if (< fg 0) theme-fg (unpack-color fg)))
  (define actual-bg (if (< bg 0) theme-bg (unpack-color bg)))

  (define base (style))
  (set-style-fg! base (if inverse? actual-bg actual-fg))
  (set-style-bg! base (if inverse? actual-fg actual-bg))

  (define bold-style (if (ghost-bit? attrs 0) (style-with-bold base) base))
  (define italic-style
    (if (ghost-bit? attrs 1) (style-with-italics bold-style) bold-style))
  (define faint-style
    (if (ghost-bit? attrs 2) (style-with-dim italic-style) italic-style))
  (define crossed-style
    (if (ghost-bit? attrs 6)
        (style-with-crossed-out faint-style)
        faint-style))
  (if (ghost-bit? attrs 8)
      (style-underline-style crossed-style Underline/Line)
      crossed-style))

(define (ghost-bit? value bit)
  (odd? (quotient value (expt 2 bit))))

(define (unpack-color value)
  (Color/rgb (quotient value 65536)
             (quotient (modulo value 65536) 256)
             (modulo value 256)))

(define (terminal-pane-area rect)
  (define total (area-height rect))
  (define max-rows (max 0 (- total 5)))
  (if (< max-rows 1)
      #f
      (begin
        (define rows
          (if *terminal-height*
              (min (max 1 *terminal-height*) max-rows)
              ;; Split the rows left after the statusline and pane border
              ;; evenly between the editor and the terminal.
              (min (max 1 (quotient (- total 2) 2)) max-rows)))
        (define pane-height (+ rows 1))
        (define y (- total pane-height))
        (define width (- (area-width rect) (right-pane-width rect)))
        (area (area-x rect) y width pane-height))))

(define (terminal-pane-area-right rect)
  (define total-width (area-width rect))
  (define max-cols (max 0 (- total-width 20)))
  (if (< max-cols 1)
      #f
      (begin
        (define cols
          (if *terminal-width*
              (min (max 1 *terminal-width*) max-cols)
              (min (max 1 (quotient (* total-width 3) 10)) max-cols)))
        (define pane-width (+ cols 1))
        (area (+ (area-x rect) (- total-width pane-width))
              (area-y rect)
              pane-width
              (area-height rect)))))

(define (right-pane-width rect)
  (define term (terminal-active-term 'right))
  (if (and term (unbox (Terminal-active term)))
      (let ([pane (terminal-pane-area-right rect)])
        (if pane (area-width pane) 0))
      0))

(define (terminal-event-handler group event)
  (define direction (TerminalTabs-direction group))
  (define state (terminal-tabs-active-term group))
  (define bottom? (eq? direction 'bottom))
  (define selected-tab
    (and bottom? (key-event? event) (terminal-select-tab-index event)))
  (cond
    [(not (and state (unbox (Terminal-active state))))
     event-result/ignore]

    [(mouse-event? event)
     (handle-mouse-event group event)]

    ;; Ctrl-\: cycle focus editor -> bottom -> right -> editor.
    [(and (key-event? event)
          (terminal-focus-editor-trigger? event))
     (terminal-cycle)
     event-result/consume]

    [(not (unbox (Terminal-focused? state)))
     event-result/ignore]

    [(and bottom? (terminal-new-tab-trigger? event))
     (new-terminal-tab direction)
     event-result/consume]

    [(and bottom? (terminal-next-tab-trigger? event))
     (switch-terminal-tab-relative direction 1)
     event-result/consume]

    [(and bottom? (terminal-prev-tab-trigger? event))
     (switch-terminal-tab-relative direction -1)
     event-result/consume]

    [selected-tab
     (switch-terminal-tab! direction selected-tab)
     event-result/consume]

    [(and (key-event? event)
          (terminal-toggle-trigger? event))
     (toggle-terminal direction)
     event-result/consume]

    ;; Ctrl-D: close the active tab (and the pane when it was the last).
    [(and (key-event? event)
          (let ([c (key-event-char event)])
            (and c
                 (char=? c #\d)
                 (ghost-bit? (or (key-event-modifier event) 0) 1))))
     (close-terminal-tab direction)
     event-result/consume]

    [(key-event? event)
     (handle-key-event state event)
     event-result/consume]

    [(paste-event? event)
     (pty-write (Terminal-pty state) (paste-event-string event))
     event-result/consume]

    [else event-result/ignore]))

(define (terminal-alt-key? event c)
  (and (key-event? event)
       (ghost-bit? (or (key-event-modifier event) 0) 2)
       (let ([actual (key-event-char event)])
         (and actual (char=? actual c)))))

(define (terminal-new-tab-trigger? event)
  (terminal-alt-key? event #\t))

(define (terminal-next-tab-trigger? event)
  (terminal-alt-key? event #\n))

(define (terminal-prev-tab-trigger? event)
  (terminal-alt-key? event #\p))

(define (terminal-select-tab-index event)
  (define c (key-event-char event))
  (and (ghost-bit? (or (key-event-modifier event) 0) 2)
       c
       (let ([code (char->integer c)])
         (and (>= code (char->integer #\1))
              (<= code (char->integer #\9))
              (- code (char->integer #\1))))))

(define (terminal-toggle-trigger? event)
  (let ([mods (or (key-event-modifier event) 0)]
        [c (key-event-char event)])
    (and c
        (or (= (char->integer c) 31)
            (and (ghost-bit? mods 1)
                 (or (char=? c #\7)
                     (char=? c #\/)))))))

(define (terminal-focus-editor-trigger? event)
  (let ([mods (or (key-event-modifier event) 0)]
        [c (key-event-char event)])
    (and c
        (or (= (char->integer c) 28)
            (and (ghost-bit? mods 1)
                 (or (char=? c #\4)
                     (char=? c #\\)))))))

(define (terminal-cursor-handler group _rect)
  (define state (terminal-tabs-active-term group))
  (if (and state
           (unbox (Terminal-active state))
           (unbox (Terminal-focused? state))
           (unbox (Terminal-cursor-shown? state)))
      (Terminal-cursor state)
      #f))

(define (handle-key-event state event)
  (define pty (Terminal-pty state))
  (define mods (or (key-event-modifier event) 0))
  (define c (key-event-char event))

  (cond
    [(key-event-backspace? event) (pty-write-char pty (integer->char 127))]
    [(key-event-enter? event) (pty-write pty "\r")]
    [(key-event-tab? event)
     (if (ghost-bit? mods 0)
         (pty-write pty "\x1b;[Z")
         (pty-write-char pty (integer->char 9)))]
    [(key-event-escape? event) (pty-write-char pty (integer->char 27))]
    [(key-event-up? event) (send-csi pty "A" mods)]
    [(key-event-down? event) (send-csi pty "B" mods)]
    [(key-event-right? event) (send-csi pty "C" mods)]
    [(key-event-left? event) (send-csi pty "D" mods)]
    [(key-event-home? event) (send-csi pty "H" mods)]
    [(key-event-end? event) (send-csi pty "F" mods)]
    [(key-event-page-up? event) (send-tilde pty "5" mods)]
    [(key-event-page-down? event) (send-tilde pty "6" mods)]
    [(key-event-insert? event) (send-tilde pty "2" mods)]
    [(key-event-delete? event) (send-tilde pty "3" mods)]
    [(key-event-F? event 1) (send-function-key pty 1)]
    [(key-event-F? event 2) (send-function-key pty 2)]
    [(key-event-F? event 3) (send-function-key pty 3)]
    [(key-event-F? event 4) (send-function-key pty 4)]
    [(key-event-F? event 5) (send-function-key pty 5)]
    [(key-event-F? event 6) (send-function-key pty 6)]
    [(key-event-F? event 7) (send-function-key pty 7)]
    [(key-event-F? event 8) (send-function-key pty 8)]
    [(key-event-F? event 9) (send-function-key pty 9)]
    [(key-event-F? event 10) (send-function-key pty 10)]
    [(key-event-F? event 11) (send-function-key pty 11)]
    [(key-event-F? event 12) (send-function-key pty 12)]
    [else (if c (send-character pty c mods) void)]))

(define (send-csi pty final mods)
  (if (= mods 0)
      (pty-write pty (string-append "\x1b;[" final))
      (pty-write pty
                 (string-append "\x1b;[1;"
                                (number->string (xterm-modifier mods))
                                final))))

(define (send-tilde pty code mods)
  (if (= mods 0)
      (pty-write pty (string-append "\x1b;[" code "~"))
      (pty-write pty
                 (string-append "\x1b;["
                                code
                                ";"
                                (number->string (xterm-modifier mods))
                                "~"))))

(define (send-function-key pty number)
  (define sequence
    (cond
      [(= number 1) "\x1b;OP"]
      [(= number 2) "\x1b;OQ"]
      [(= number 3) "\x1b;OR"]
      [(= number 4) "\x1b;OS"]
      [(= number 5) "\x1b;[15~"]
      [(= number 6) "\x1b;[17~"]
      [(= number 7) "\x1b;[18~"]
      [(= number 8) "\x1b;[19~"]
      [(= number 9) "\x1b;[20~"]
      [(= number 10) "\x1b;[21~"]
      [(= number 11) "\x1b;[23~"]
      [(= number 12) "\x1b;[24~"]
      [else #f]))
  (when sequence (pty-write pty sequence)))

(define (send-character pty c mods)
  (define alt? (ghost-bit? mods 2))
  (define ctrl? (ghost-bit? mods 1))
  (define control (if ctrl? (ctrl-char c) #f))
  (when alt? (pty-write-char pty (integer->char 27)))
  (cond
    [control (pty-write-char pty control)]
    [else (pty-write-char pty c)]))

(define (ctrl-char c)
  (define code (char->integer c))
  (cond
    [(and (>= code 65) (<= code 90)) (integer->char (- code 64))]
    [(and (>= code 97) (<= code 122)) (integer->char (- code 96))]
    [(= code 32) (integer->char 0)]
    [(= code 63) (integer->char 127)]
    [else #f]))

(define (xterm-modifier mods)
    (+ 1
        (if (ghost-bit? mods 0) 1 0)
        (if (ghost-bit? mods 2) 2 0)
        (if (ghost-bit? mods 1) 4 0)
        (if (ghost-bit? mods 3) 8 0)))

(define (toggle-terminal direction)
  (define term (terminal-active-term direction))
  (if (and term (unbox (Terminal-active term)))
      (hide-terminal direction)
      (show-terminal direction)))

(define (hide-terminal direction)
  (define term (terminal-active-term direction))
  (when term
    (define was-focused? (unbox (Terminal-focused? term)))
    (set-box! (Terminal-focused? term) #f)
    (set-box! (Terminal-active term) #f)
    (when was-focused?
      (when (eq? *focused-direction* direction)
        (set! *focused-direction* #f))
      (restore-editor-cursor!)
      (auto-reload-editor!))
    (set-editor-clip! direction 0)
    (helix.redraw)))

(define (close-terminal-tab direction)
  (define group (terminal-get direction))
  (when group
    (let* ([tabs (terminal-tabs-list group)]
           [index (terminal-tabs-active-index group)])
      (when (< index (length tabs))
        (let* ([term (list-ref tabs index)]
               [was-focused? (unbox (Terminal-focused? term))])
          (set-box! (Terminal-active term) #f)
          (set-box! (Terminal-focused? term) #f)
          (set-box! (Terminal-cursor-shown? term) #f)
          (set-box! (Terminal-kill-switch term) #t)
          (pty-kill (Terminal-pty term))
          (let ([remaining (remove-list-at tabs index)])
            (set-box! (TerminalTabs-tabs group) remaining)
            (if (null? remaining)
                (begin
                  (when (unbox (TerminalTabs-pushed? group))
                    (pop-last-component! (component-name direction))
                    (set-box! (TerminalTabs-pushed? group) #f))
                  (terminal-remove! direction)
                  (when was-focused?
                    (when (eq? *focused-direction* direction)
                      (set! *focused-direction* #f))
                    (restore-editor-cursor!)
                    (auto-reload-editor!))
                  (set-editor-clip! direction 0))
                (begin
                  (set-box! (TerminalTabs-active group)
                            (min index (- (length remaining) 1)))
                  (let ([new-term (terminal-tabs-active-term group)])
                    (set-box! (Terminal-active new-term) #t)
                    (set-box! (Terminal-focused? new-term) was-focused?)))))
          (helix.redraw))))))

(define (auto-save-editor!)
  (let ([doc (editor->doc-id (editor-focus))])
    (when (and (editor-document->path doc)
               (editor-document-dirty? doc))
      (helix.write))))

(define (auto-reload-editor!)
  (unless (editor-has-dirty-documents?)
    (helix.reload-all)))

(define (editor-has-dirty-documents?)
  (let loop ([docs (editor-all-documents)])
    (cond
      [(null? docs) #f]
      [(editor-document-dirty? (car docs)) #t]
      [else (loop (cdr docs))])))

(define *editor-cursor-shapes* #f)

(define (save-editor-cursor-shapes!)
  (unless *editor-cursor-shapes*
    (set! *editor-cursor-shapes*
      (list
        (string->symbol
          (get-config-option-value "cursor-shape.normal"))
        (string->symbol
          (get-config-option-value "cursor-shape.insert"))
        (string->symbol
          (get-config-option-value "cursor-shape.select"))))))

(define (hide-editor-cursor!)
  (cursor-shape
    #:normal 'hidden
    #:insert 'hidden
    #:select 'hidden))

(define (restore-editor-cursor!)
  (when *editor-cursor-shapes*
    (cursor-shape
      #:normal (list-ref *editor-cursor-shapes* 0)
      #:insert (list-ref *editor-cursor-shapes* 1)
      #:select (list-ref *editor-cursor-shapes* 2))))

(define terminal-run-config
  '(
    ("rust"
      ("main.rs" . "cargo run")
      ("*" . "cargo build"))

    ("go"
      ("main.go" . "go run *.go")
      ("*" . "go build"))
    ))

(define (terminal-run language buffer-name)
  (define language* (terminal-expand-argument language))
  (define buffer-name* (terminal-expand-argument buffer-name))
  (define command (terminal-run-command language* buffer-name*))
  (if command
      (terminal-exec-in 'bottom command)
      (set-status!
        (string-append "No run command configured for " language*))))

(define (terminal-expand-argument value)
  (let ([result (expand value)])
    (if (hash-ref result 'ok)
        (hash-ref result 'text)
        value)))

(define (terminal-run-command language buffer-name)
  (define basename (path-basename buffer-name))

  (define language-config
    (assoc language terminal-run-config))

  (and language-config
       (let* ([rules (cdr language-config)]
              [rule (or (assoc basename rules)
                        (assoc "*" rules))])
         (and rule (cdr rule)))))

(define (path-basename path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) path]
      [(char=? (string-ref path i) #\/)
       (substring path (+ i 1) (string-length path))]
      [else (loop (- i 1))])))

(define (terminal-exec-in direction command)
  (define result (expand command))
  (if (not (hash-ref result 'ok))
    (set-status! (hash-ref result 'error))
    (let ([command (hash-ref result 'text)])
      (if (string=? command "")
        (set-status! "terminal-exec: missing command")
        (begin
          (show-terminal direction)
          (let ([term (terminal-active-term direction)])
            (when term
              (pty-write
                (Terminal-pty term)
                (string-append command "\r"))
              (helix.redraw))))))))

(define (terminal-exec-command args)
  (string-join args " "))

(define (mouse-inside-terminal? state event)
  (define pane (unbox (Terminal-area state)))
  (and pane
       (>= (event-mouse-col event) (area-x pane))
       (< (event-mouse-col event) (+ (area-x pane) (area-width pane)))
       (>= (event-mouse-row event) (area-y pane))
       (< (event-mouse-row event) (+ (area-y pane) (area-height pane)))))

(define (handle-mouse-event group event)
  (define direction (TerminalTabs-direction group))
  (define state (terminal-tabs-active-term group))
  (define kind (event-mouse-kind event))
  (define pane (and state (unbox (Terminal-area state))))
  (define tab-index
    (and pane
         (eq? direction 'bottom)
         (= kind 0)
         (= (event-mouse-row event) (area-y pane))
         (tab-index-at-column group pane (event-mouse-col event))))
  (cond
    [(or (not state) (unbox (Terminal-kill-switch state)))
     event-result/ignore]

    [tab-index
     (switch-terminal-tab! direction tab-index)
     (focus-terminal! direction)
     (helix.redraw)
     event-result/consume]

    ;; Clicks outside the pane belong to the editor. If the terminal had
    ;; focus, let go of it so the editor receives keys again.
    [(not (mouse-inside-terminal? state event))
     (when (unbox (Terminal-focused? state))
       (blur-terminal!))
     event-result/ignore]

    [else
     ;; Focus on presses and wheel events, but not on releases: the release
     ;; that follows opening a path must not steal focus back from the editor.
     (when (or (<= kind 2) (= kind 10) (= kind 11))
       (focus-terminal! direction))
     (cond
       [(= kind 10) (vte/scroll-down (Terminal-vte state) 3)]
       [(= kind 11) (vte/scroll-up (Terminal-vte state) 3)]
       [(= kind 0) (open-path-at-click state event)]
       [else void])
     (helix.redraw)
     event-result/consume]))

;; Reconstruct the visible text of one terminal row. Cells are one column
;; wide, so pad blanks with spaces to keep the click column aligned.
(define (terminal-row-text vt target-row cols)
  (define cells (make-vector cols " "))
  (vte/reset-iterator! vt)
  (while (vte/advance-iterator! vt)
    (when (and (= (vte/iter-y vt) target-row)
               (>= (vte/iter-x vt) 0)
               (< (vte/iter-x vt) cols))
      (let ([text (vte/iter-cell-str vt)])
        (unless (string=? text "")
          (vector-set! cells (vte/iter-x vt) text)))))
  (string-join (mutable-vector->list cells) ""))

(define (token-under-click line x)
  (define n (string-length line))
  (if (or (>= x n) (char-whitespace? (string-ref line x)))
      #f
      (begin
        (define start x)
        (while (and (> start 0)
                    (not (char-whitespace? (string-ref line (- start 1)))))
          (set! start (- start 1)))
        (define end x)
        (while (and (< end n)
                    (not (char-whitespace? (string-ref line end))))
          (set! end (+ end 1)))
        (substring line start end))))

(define (strip-token text)
  (define start 0)
  (define end (string-length text))
  (while (and (< start end) (leading-delimiter? (string-ref text start)))
    (set! start (+ start 1)))
  (while (and (> end start) (trailing-delimiter? (string-ref text (- end 1))))
    (set! end (- end 1)))
  (substring text start end))

(define (leading-delimiter? c)
  (or (char=? c #\() (char=? c #\[) (char=? c #\{)
      (char=? c #\<) (char=? c #\") (char=? c #\')
      (char=? c #\`)))

(define (trailing-delimiter? c)
  (or (char=? c #\)) (char=? c #\]) (char=? c #\})
      (char=? c #\>) (char=? c #\") (char=? c #\')
      (char=? c #\`) (char=? c #\,) (char=? c #\;)))

(define (colon-parts text)
  (define parts '())
  (define start 0)
  (define i 0)
  (while (< i (string-length text))
    (when (char=? (string-ref text i) #\:)
      (set! parts (cons (substring text start i) parts))
      (set! start (+ i 1)))
    (set! i (+ i 1)))
  (reverse (cons (substring text start (string-length text)) parts)))

(define (digits-only? text)
  (define ok (> (string-length text) 0))
  (define i 0)
  (while (< i (string-length text))
    (define code (char->integer (string-ref text i)))
    (when (or (< code 48) (> code 57))
      (set! ok #f))
    (set! i (+ i 1)))
  ok)

(define (plausible-path? path)
  (and (> (string-length path) 0)
       (or (string-contains? path "/")
           (string-contains? path "\\")
           (string-contains? path "."))))

;; Turn a token like src/main.rs:10:5 into the same location string that
;; Helix's :open command understands. Trailing colons/commas are ignored.
(define (location-at-token token)
  (define cleaned (strip-token token))
  (if (string=? cleaned "")
      #f
      (let ([rev (reverse (colon-parts cleaned))])
        (while (and (not (null? rev)) (string=? (car rev) ""))
          (set! rev (cdr rev)))
        (if (or (null? rev) (null? (cdr rev)))
            #f
            (let ([last (car rev)]
                  [rest (cdr rev)])
              (if (not (digits-only? last))
                  #f
                  (let ([prev (car rest)])
                    (if (digits-only? prev)
                        (let ([path (string-join (reverse (cdr rest)) ":")])
                          (if (plausible-path? path)
                              (string-append path ":" prev ":" last)
                              #f))
                        (let ([path (string-join (reverse rest) ":")])
                          (if (plausible-path? path)
                              (string-append path ":" last)
                              #f))))))))))

;; Left-click a path:line:col shown in the terminal and open it in the
;; editor. The terminal stays visible, but focus returns to the editor.
(define (open-path-at-click state event)
  (define pane (unbox (Terminal-area state)))
  (define size (unbox (Terminal-size state)))
  (when (and pane size)
    (define rows (car size))
    (define cols (car (cdr size)))
    (define right? (eq? (Terminal-direction state) 'right))
    (define origin-x (+ (area-x pane) (if right? 1 0)))
    (define origin-y (+ (area-y pane) (if right? 0 1)))
    (define cell-x (- (event-mouse-col event) origin-x))
    (define cell-y (- (event-mouse-row event) origin-y))
    (when (and (>= cell-x 0) (< cell-x cols)
               (>= cell-y 0) (< cell-y rows))
      (let* ([line (terminal-row-text (Terminal-vte state) cell-y cols)]
             [token (token-under-click line cell-x)])
        (when token
          (let ([location (location-at-token token)])
            (when location
              (helix.open location)
              (blur-terminal!))))))))
