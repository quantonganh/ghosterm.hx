(require "helix/editor.scm")

(provide run-command
         run-command-for
         run-config)

(define run-config
  '(
    ("go" "go run *.go")
    ("rs" "cargo run")
    ))

;; Find the last occurrence of a character.
(define (string-last-index str ch)
  (let loop ([i (- (string-length str) 1)])
    (cond
      [(< i 0) #f]
      [(char=? (string-ref str i) ch) i]
      [else (loop (- i 1))])))

;; Filename including extension.
(define (path-basename path)
  (let ([i (string-last-index path #\/)])
    (if i
        (substring path (+ i 1) (string-length path))
        path)))

;; Directory containing the file.
(define (path-dirname path)
  (let ([i (string-last-index path #\/)])
    (cond
      [(not i) "."]
      [(= i 0) "/"]
      [else (substring path 0 i)])))

;; File extension.
(define (path-extension path)
  (let* ([name (path-basename path)]
         [i (string-last-index name #\.)])
    (if (and i (> i 0))
        (substring name (+ i 1) (string-length name))
        "")))

;; Filename without extension.
(define (path-stem path)
  (let* ([name (path-basename path)]
         [i (string-last-index name #\.)])
    (if (and i (> i 0))
        (substring name 0 i)
        name)))

(define (shell-quote value)
  (string-append
    "'"
    (string-replace value "'" "'\\''")
    "'"))

(define (expand-template template path)
  (let* ([basedir (path-dirname path)]
         [filename (path-basename path)]
         [stem (path-stem path)])
    (string-replace
      (string-replace
        (string-replace
          (string-replace
            template
            "$buffer_name"
            (shell-quote path))
          "$basedir"
          (shell-quote basedir))
        "$file_name"
        (shell-quote filename))
      "$file_stem"
      (shell-quote stem))))

(define (find-config extension)
  (let loop ([entries run-config])
    (cond
      [(null? entries)
       #f]

      [(equal? extension (car (car entries)))
       (car entries)]

      [else
       (loop (cdr entries))])))

(define (current-path)
  (let* ([focus (editor-focus)]
         [doc-id (editor->doc-id focus)])
    (editor-document->path doc-id)))

(define (run-command-for action path)
  (if (not (equal? action "run"))
      #f
      (let ([entry (find-config (path-extension path))])
        (if entry
            (expand-template (cadr entry) path)
            #f))))

(define (run-command action)
  (run-command-for action (current-path)))
