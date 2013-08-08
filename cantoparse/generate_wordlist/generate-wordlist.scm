#! /usr/local/bin/csi -script

; Translates a dictionary file into wordlist.c for cantoparse

; Works with Chicken Scheme 4.7.0 w/ anaphora egg.
; It's quite an ugly code, needs a rewrite. But it needs to run only once.

; It should work with both original CEDICT format and Cantonese CC-EDICT format
; (with 2 transcription fields).  If used with Cantonese dictionary, only the first
; transcription is used.

; Please note that Cantonese CC-EDICT's transcriptions are a mess (sometimes
; tones are indicated with diacritics, sometimes with numbers).

(use posix irregex srfi-69 srfi-95)

(define input-filename (if (not (null? (command-line-arguments)))
                           (car (command-line-arguments))
                           #f))

(define output-filename (if (>= (length (command-line-arguments)) 2)
                            (cadr (command-line-arguments))
                            #f))


(define (call-with-files input-filename output-filename function)
  (define input-port (if input-filename
                         (open-input-file input-filename)
                         (current-input-port)))
  (define output-port (if output-filename
                          (open-output-file output-filename)
                          (current-output-port)))
  (function input-port output-port)
  (if input-filename (close-input-port input-port))
  (if output-filename (close-output-port output-port)))

;; returns a list of 3 elements: ((trad [simp]) transcription (translation1 translation2 ...)
;; returns false if line is invalid
(define (parse-cedict-line line)
  (letrec ((hanzi '(seq (submatch (+ (~ #\ ))) " "))
           (transcription '(seq "[" (submatch (* (~ "/]"))) "] "))
           (translations '(seq "/" (submatch (+ any)) "/" (* whitespace)))
           (re `(seq bos ,hanzi ,hanzi ,transcription (* ,transcription) ,translations eos))
           (match (irregex-match re line)))
    (if match
        (let ((trad (irregex-match-substring match 1))
              (simp (irregex-match-substring match 2))
              (transcription (irregex-match-substring match 3))
              (translations (string-split (irregex-match-substring match 5) "/")))
           (list (if (string=? trad simp) (list trad) (list trad simp))
                 transcription
                 translations))
        #f)))

(define (format-string data)
  (string-append "{ \""
                 (string-intersperse (map (lambda (x)
                                            (string-append "<["
                                                           (preformat-string x)
                                                           "]>"))
                                          (append (list (cadr data))
                                                  (caddr data)))
                                     "")
                 "\"}"))

(define (preformat-string s)
  (irregex-replace/all '(seq "\"") s "\\\""))

(define (display-writings w port)
  (define first? #t)
  (for-each (lambda (x)
                     (if first? (set! first? #f)
                                (display ","))
                     (display (string-append "\n  {"
                                             (number->string (cdr x))
                                              ", \""
                                             (preformat-string (car x))
                                             "\" }")
                              out))
            (sort! (hash-table->alist w) string<=? car)))

(define (process-file in out)
  (define writings (make-hash-table))
  (display "#include \"common.h\"" out)
  (newline out)
  (display "#include \"wordlist.h\"" out)
  (newline out)
  (newline out)
  (display "CantoneseDefinition definitions[] = {" out)
  (newline out)
  (let loop ((line (read-line in))
             (current-id 0)
             (first-line #t))
    (if (eof-object? line)
        #t
        (let ((data (parse-cedict-line line)))
          (if data
              (begin (if (not first-line)
                         (display ",\n" out))
                     (for-each (lambda (x)
                                 (hash-table-set! writings x current-id))
                               (car data))
                     (display (format-string data) out)
                     (loop (read-line in) (+ current-id 1) #f))
              (loop (read-line in) current-id first-line)))))
  (display "\n};\n\nWrittenForm written_forms[] = {\n")
  (display-writings writings out)
  (display "\n};")
  (newline out)
  (display "int definitions_length = LENGTH(definitions);" out)
  (newline out)
  (display "int written_forms_length = LENGTH(written_forms);" out)
  (newline out)
  )

(call-with-files input-filename output-filename process-file)
