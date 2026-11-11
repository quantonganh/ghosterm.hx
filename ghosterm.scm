(require "helix/editor.scm")
(require "helix/configuration.scm")
(require "helix/misc.scm")
(require "helix/components.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require "helix/editor.scm")
(require "run.scm")

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
         terminal-exec
         terminal-run
         terminal-toggle)

(define *terminal* #f)
(define *terminal-shell* (default-shell))

;; Number of text rows the bottom pane occupies (the top border is extra).
;; #f means "half of the current editor height", recomputed on every render.
(define *terminal-height* #f)

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

(define (show-terminal)
  (unless (terminal-loop-alive? *terminal*)
    (when (and *terminal*
               (unbox (Terminal-pushed? *terminal*)))
      (pop-last-component! "ghosterm"))

    (set! *terminal*
          (make-terminal *terminal-shell*
                         (or *terminal-height* (default-terminal-height))
                         80))

    (terminal-loop *terminal*))

  (define term *terminal*)

  (set-box! (Terminal-active term) #t)
  (set-box! (Terminal-focused? term) #t)

  (save-editor-cursor-shapes!)
  (hide-editor-cursor!)

  (unless (unbox (Terminal-pushed? term))
    (set-box! (Terminal-pushed? term) #t)
    (push-component!
     (new-component! "ghosterm"
                     term
                     (Terminal-renderer term)
                     (component-functions term))))

  (helix.redraw))

(define (component-functions term)
    (define with-handle
        (hash-insert (hash) "handle_event" (Terminal-event-handler term)))
    (hash-insert with-handle "cursor" (Terminal-cursor-handler term)))

(define (terminal-loop-alive? term)
  (and term (not (unbox (Terminal-kill-switch term)))))

(define (focus-terminal!)
  (when *terminal*
    (set-box! (Terminal-focused? *terminal*) #t)))

(struct Terminal
        (name cursor pty vte size focused? active pushed? kill-switch area
              cursor-shown? renderer event-handler cursor-handler))

(define (make-terminal shell rows cols)
  (define pty (create-pty shell rows cols))
  (define vt (virtual-terminal pty rows cols))
  (Terminal "ghosterm"
            (position 0 0)
            pty
            vt
            (box #f)
            (box #f)
            (box #f)
            (box #f)
            (box #f)
            (box #f)
            (box #f)
            terminal-render
            terminal-event-handler
            terminal-cursor-handler))

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
            (set-box! kill-switch #t)
            (set-box! (Terminal-focused? term) #f)
            (helix.redraw)])))))

(define (terminal-render state rect frame)
  (when state
    (if (unbox (Terminal-active state))
        (render-bottom-terminal state rect frame)
        (set-editor-clip-bottom! 0))))

(define (render-bottom-terminal state rect frame)
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

     (when (> (area-width pane) 16)
       (frame-set-string! frame
                          (+ (area-x pane) 2)
                          (area-y pane)
                          " terminal "
                          border-style))

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
        (area (area-x rect) y (area-width rect) pane-height))))

(define (terminal-event-handler state event)
  (cond
    [(not (unbox (Terminal-active state)))
     event-result/ignore]

    [(mouse-event? event)
     (handle-mouse-event state event)]

    [(not (unbox (Terminal-focused? state)))
     event-result/ignore]

    [(and (key-event? event)
          (terminal-toggle-trigger? event))
     (toggle-terminal)
     event-result/consume]

    ;; Esc: return focus to editor.
    [(and (key-event? event)
          (key-event-escape? event))
     (set-box! (Terminal-focused? state) #f)
     (restore-editor-cursor!)
     (helix.redraw)
     event-result/consume]

    [(key-event? event)
     (handle-key-event state event)
     event-result/consume]

    [(paste-event? event)
     (pty-write (Terminal-pty state) (paste-event-string event))
     event-result/consume]

    [else event-result/ignore]))

(define (terminal-toggle-trigger? event)
  (let ([mods (or (key-event-modifier event) 0)]
        [c (key-event-char event)])
    (and c
        (or (= (char->integer c) 31)
            (and (ghost-bit? mods 1)
                 (or (char=? c #\7)
                     (char=? c #\/)))))))

(define (terminal-cursor-handler state _rect)
  (if (and (unbox (Terminal-active state))
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

(define (toggle-terminal)
    (define term *terminal*)
    (if (and term (unbox (Terminal-active term)))
        (hide-terminal)
        (show-terminal)))

(define (hide-terminal)
    (define term *terminal*)
    (set-box! (Terminal-focused? term) #f)
    (set-box! (Terminal-active term) #f)
    (restore-editor-cursor!)
    (set-editor-clip-bottom! 0)
    (helix.redraw))

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

(define (terminal-run)
  (let ([command (run-command "run")])
    (when command
      (terminal-exec command))))

(define (terminal-exec . args)
  (define command (terminal-exec-command args))
  (if (string=? command "")
    (set-status! "terminal-exec: missing command")
    (begin
      (show-terminal)
      (let ([term *terminal*])
        (when term
          (pty-write
            (Terminal-pty term)
            (string-append command "\r"))

          (set-box! (Terminal-focused? term) #t)
          (helix.redraw))))))

(define (terminal-exec-command args)
  (string-join args " "))

(define (mouse-inside-terminal? state event)
  (define pane (unbox (Terminal-area state)))
  (and pane
       (>= (event-mouse-col event) (area-x pane))
       (< (event-mouse-col event) (+ (area-x pane) (area-width pane)))
       (>= (event-mouse-row event) (area-y pane))
       (< (event-mouse-row event) (+ (area-y pane) (area-height pane)))))

(define (handle-mouse-event state event)
  (define kind (event-mouse-kind event))
  (cond
    [(unbox (Terminal-kill-switch state))
     event-result/ignore]

    ;; Clicks outside the pane belong to the editor. If the terminal had
    ;; focus, let go of it so the editor receives keys again.
    [(not (mouse-inside-terminal? state event))
     (when (unbox (Terminal-focused? state))
       (set-box! (Terminal-focused? state) #f)
       (helix.redraw))
     event-result/ignore]

    [else
     ;; Focus on presses and wheel events, but not on releases: the release
     ;; that follows opening a path must not steal focus back from the editor.
     (when (or (<= kind 2) (= kind 10) (= kind 11))
       (focus-terminal!))
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
    (define cell-x (- (event-mouse-col event) (area-x pane)))
    (define cell-y (- (event-mouse-row event) (+ (area-y pane) 1)))
    (when (and (>= cell-x 0) (< cell-x cols)
               (>= cell-y 0) (< cell-y rows))
      (let* ([line (terminal-row-text (Terminal-vte state) cell-y cols)]
             [token (token-under-click line cell-x)])
        (when token
          (let ([location (location-at-token token)])
            (when location
              (helix.open location)
              (set-box! (Terminal-focused? state) #f))))))))
