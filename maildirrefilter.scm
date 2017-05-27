#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))
(use-modules  (ice-9 format))
(define *maildir* "/home/joakim/Maildir")

(define (main args)
  (map (lambda (arg) (display arg) (display " "))
       (cdr args))
  (begin (map process-mail (directory-files-2  (string-append *maildir* "/cur"))) '())
  (format #t "\n\n ~d messages moved\n" *msgmoved*)
  )

(define (directory-files dir)
  "return dir content as list(from ftw.scm)"
  (let ((dir-stream (opendir dir)))
    (let loop ((new (readdir dir-stream))
               (acc '()))
      (if (eof-object? new)
	  (begin
	    (closedir dir-stream)
	    acc)
          (if (or (string=? "."  new)             ;;; ignore
                  (string=? ".." new))            ;;; ignore
              acc
              (cons new acc)))
      acc)))

;;because directory-file failed to work
(define (directory-files-2 dirname)
  (let ((dir (opendir dirname))
        (acc '()))

    (do ((entry (readdir dir) (readdir dir)))
        ((eof-object? entry))
      (if  (not  (or (string=? "."  entry)             
                     (string=? ".." entry)))
           (set! acc (append acc (list  (format  #f "~a/~a" dirname entry))))) )
                                        ;(display acc)(newline)

    
    (closedir dir)
    acc))





;;cd /home/joakim/Maildir/cur
;;for x in *; do sieve-test 2>&1 ~/.dovecot.sieve $x|grep -P 'store message in folder..(?!INBOX)';done

(define (test-mail file)
  "see if the mail will be filtered by sieve. returns the box to filter to, or #f"
  (let* ((port  (open-input-pipe  (format #f "sieve-test  ~~/.dovecot.sieve ~s 2>&1" file)))
         (str (read-line port) )
         (str (read-line port) )
         (str (read-line port) )
         (str2 (read-line port) )
         (match  (string-match "store message in folder:.(.*)" str2)))
    (close-pipe port)
    (if match
        (match:substring match 1)
        #f)
    )
  )

(define (test-mail-rspamc file)
  "see if the mail will be filtered by rspamc. returns the box to filter to, or #f"
  (let* ((port  (open-input-pipe  (format #f "rspamc ~s 2>&1" file)))
         (str (read-line port) )
         (str (read-line port) )
         (str2 (read-line port) )
         (match  (string-match "Spam: true" str2)))
    (close-pipe port)
    (if match
        (begin
          (match:substring match 0)
          (format #t "rspamc triggered: ~s" file)
          "rspamd"
          )
        #f
        )
    ;;#f ;;always false for now
    )
  )
(define *msgcount* 0)
(define *msgmoved* 0)

(define (process-mail file)
  (let ((destination (test-mail file))) ;;test-mail-rspamc if doing spam, otherwise just test-spam. test-mail-rspamc is very slow atm
    ;;check (access? destination)
    (format #t ".")
    (set! *msgcount* (+ 1 *msgcount*))
    (if (= 0 (modulo *msgcount* 80 ))
        (begin
          (format #t " ~d\n" *msgcount*)
          ))
    (if destination
        (let* ( (destination-path (format #f "~a/.~a/cur/" *maildir* destination))
                (access-path (access? destination-path W_OK)))
          (if (not access-path)
              (begin
                (format #t "folder not found, creating")
                (system (format  #f "mkdir -p ~s\n"  destination-path))
                )
              )
          (format #t "\n")
          (format  #t "~amv ~s  ~s\n" (if access-path "" "#") file destination-path)
          ;;TODO the "mv" might fail
          (if (not (equal?   0 (system (format  #f "~amv ~s  ~s\n" "" file destination-path))))
              (exit 1))
          (set! *msgmoved* (+ 1 *msgmoved*)))
        
        ;;(format  #t "leave ~s\n" file )
        ))
  )

;;(test-mail "/home/joakim/Maildir/cur/1300220279.M979683P2628.chopper,S=10309,W=10486:2,S")

                                        ;(map test-mail (directory-files-2  "/home/joakim/Maildir/cur"))
                                        ;(begin (map process-mail (directory-files-2  "/home/joakim/Maildir/cur")) '())
