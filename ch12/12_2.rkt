;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12_2) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(define ITUNES-LOCATION "E:\\itunes.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

; ex199
(define today (create-date 2021 7 17 10 58 0))
(define music (create-track
               "claire de lune"
               "matt bacon"
               "guitar"
               300000
               1
               today
               1
               today))
(define testt (list music))

; ex200
; LTracks -> Number
; produces total amount of play time(second) for a give list of tracks
(define (total-time lt)
  (cond
    [(empty? lt) 0]
    [else (+ (single-time (first lt))
             (total-time (rest lt)))]))
(check-expect (total-time testt) (single-time (first testt)))


; Track -> Number
; produces amount of play time(s) for a single track
(define (single-time t)
  (* (track-time t) (track-play# t)))
(check-expect (single-time music) (* (track-time music) (track-play# music)))

; ex201
; LTracks -> List-of-strings
; produce the list of album titles
(define (select-all-album-titles lt)
  (cond
    [(empty? lt) '()]
    [else (cons (track-album (first lt))
                (select-all-album-titles (rest lt)))]))
(check-expect (select-all-album-titles testt)
              (list "guitar"))

; List-of-strings -> List-of-strings
; contains every string from given list exactly once
(define (create-set los)
  (cond
   [(empty? los) '()]
   [else (set+ (create-set (rest los)) (first los))]))

; List-of-strings String -> List-of-strings
; add String to set unless it already exists
(define (set+ s i)
  (if (in? s i) s (cons i s)))
(check-expect (list (set+ (list "hello" "world") "hello")
                    (set+ (list "world") "hello"))
              (list (list "hello" "world")
                    (list "hello" "world")))

; List-of-strings String -> Bool
; check if string already exists
(define (in? s i)
  (cond
    [(empty? s) #false]
    [else (or (string=? (first s) i)
              (in? (rest s) i))]))
(check-expect (list (in? (list "hello" "world") "hello")
                    (in? (list "world") "hello"))
              (list #true
                    #false))

; ex201
; LTracks -> List-of-strings
; produce the list of album titles that each title only occurs once
(define (select-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))

; ex202
; LTracks String -> LTracks
(define (select-album lt album)
  (cond
    [(empty? lt) '()]
    [else (if (string=? (track-album (first lt)) album)
              (cons (first lt) (select-album (rest lt) album))
              (select-album (rest lt) album))]))

; ex203
; LTracks String Date -> LTracks
(define (select-album-date lt album date)
  (select-date (select-album lt album) date))

(define (select-date lt date)
  (cond
    [(empty? lt) '()]
    [else (if (date> (track-played (first lt)) date)
              (cons (first lt) (select-date (rest lt) date))
              (select-date (rest lt) date))]))

(define (date> d1 d2)
  (cond
    [(> (date-year d1) (date-year d2)) #true]
    [(< (date-year d1) (date-year d2)) #false]
    [else
     (cond
       [(> (date-month d1) (date-month d2)) #true]
       [(< (date-month d1) (date-month d2)) #false]
       [else
        (cond
          [(> (date-day d1) (date-day d2)) #true]
          [(< (date-day d1) (date-day d2)) #false]
          [else
           (cond
             [(> (date-hour d1) (date-hour d2)) #true]
             [(< (date-hour d1) (date-hour d2)) #false]
             [else
              (cond
                [(> (date-minute d1) (date-minute d2)) #true]
                [(< (date-minute d1) (date-minute d2)) #false]
                [else
                 (cond
                   [(> (date-second d1) (date-second d2)) #true]
                   [else #false])])])])])]))

; ex204
(define (select-albums lt)
  (seperate-albums lt (select-album-titles/unique lt)))

(define (seperate-albums lt albums)
  (cond
    [(empty? albums) '()]
    [else (cons (select-album lt (first albums))
                (seperate-albums lt (rest albums)))]))


; -------------------------------------------------------------
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))
; ex205
; ...
; ex206
; LAssoc String Any -> default
(define (find-association l key default)
  (cond
    [(empty? l) default]
    [else (if (equal? (first (first l)) key)
              (first (rest (first l)))
              (find-association (rest l) key default))]))

(define (total-time/list l)
  (cond
    [(empty? l) 0]
    [else (+ (* (find-association (first l) "Total Time" 0)
                (find-association (first l) "Play Count" 0))
             (total-time/list (rest l)))]))
(check-expect (total-time/list list-tracks)
              (total-time itunes-tracks))

(define (boolean-attributes l)
  (cond
    [(empty? l) '()]
    [else (if (empty? (find-bool (first l)))
              (boolean-attributes (rest l))
              (set+ (boolean-attributes (rest l)) (find-bool (first l))))]))

; LAssoc -> List-of-strings
(define (find-bool l)
  (cond
    [(empty? l) '()]
    [else (if (boolean? (first (rest (first l))))
              (first (first l))
              (find-bool (rest l)))]))

; LAssoc -> Track
(define (track-as-struct as)
  (if (valid-track? as)
      (create-track (find-association as "Name" '())
                    (find-association as "Artist" '())
                    (find-association as "Album" '())
                    (find-association as "Total Time" '())
                    (find-association as "Track Number" '())
                    (find-association as "Date Added" '())
                    (find-association as "Play Count" '())
                    (find-association as "Play Date UTC" '()))
      #false))

(define (valid-track? as)
  (and (string? (find-association as "Name" '()))
       (string? (find-association as "Artist" '()))
       (string? (find-association as "Album" '()))
       (number? (find-association as "Total Time" '()))
       (number? (find-association as "Track Number" '()))
       (number? (find-association as "Play Count" '()))
       (date? (find-association as "Date Added" '()))
       (date? (find-association as "Play Date UTC" '()))))