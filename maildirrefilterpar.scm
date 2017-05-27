#!    /opt/guile-2.2.0/bin/guile \
-e main -s
!#
;; export GUIX_LOCPATH=/opt/guile-2.2.0/lib/locale in
(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))
(use-modules  (ice-9 format))
(use-modules (ice-9 threads))

(use-modules (ice-9 atomic)) ;;needs guile 2.2

(use-modules (ice-9 getopt-long))

(define *maildir* "/home/joakim/Maildir")

(define *msgcount* (make-atomic-box 0))
(define *msgmoved* (make-atomic-box 0))
(define *printlock* (make-mutex))

(define (incatom atom)
  ;;a named let instead?
  (let ((expected (atomic-box-ref atom)))
    (while (not (= expected (atomic-box-compare-and-swap! atom expected (+ 1 expected))))
            (set! expected (atomic-box-ref atom)))
    expected))


(define (progress-report)
  "thread safe progress report"
  (with-mutex *printlock*
    (format #t ".")
    ;;now theres both atomics and mutexes, and i guess a mutex would be enough.
    (let ((msgcount (incatom *msgcount*)))
      (if (= 0 (modulo  msgcount 100 ))
          (begin
            (format #t " ~d\n"   msgcount))))))

(define *dryrun* #t)

(define (drysystem arg)
  (if (not *dryrun*)
      (system arg)
      0))
(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (help    (single-char #\h) (value #f))
                        (dryrun  (single-char #\d) (value #t))
                        (rspamc  (single-char #\r) (value #t))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (rspamc (option-ref options 'rspamc #f)))
    (set! *dryrun* (not (string=? "0" (option-ref options 'dryrun "1"))))
    (format #t "dryrun: ~s rspamc: ~s\n" *dryrun* rspamc)
    (if (or version-wanted help-wanted)
        (begin
          (if version-wanted
              (display "maildirrefilterpar version 0.3\n"))
          (if help-wanted
              (display "\
getopt-long-example [options]
  -v, --version    Display version
  -h, --help       Display this help
  -d, --dryrun     0 means no dry run, run for real
  -r, --rspam      apart from sieve, also test with rspamc
")))
        
        (begin
          (map (lambda (arg) (display arg) (display " "))
               (cdr args))
          (format #t "length of dirctory files:  ~d \n" (length (directory-files-2  (string-append *maildir* "/cur"))))
          (begin (n-par-map 64 (lambda (file) (process-mail file rspamc))
                            (directory-files-2  (string-append *maildir* "/cur"))
                            ) '())
          (atomic-box-ref *msgcount*)
          (format #t "\n\n ~d messages processed\n" (atomic-box-ref *msgcount*))
          (format #t "\n\n ~d messages moved\n"  (atomic-box-ref *msgmoved*))))))

;;because directory-file failed to work, i made a local variant
(define (directory-files-2 dirname)
  (let ((dir (opendir dirname))
        (acc '()))

    (do ((entry (readdir dir) (readdir dir)))
        ((eof-object? entry))
      (if  (not  (or (string=? "."  entry)             
                     (string=? ".." entry)))
           (set! acc (append acc (list  (format  #f "~a/~a" dirname entry))))) )
    (closedir dir)
    acc))



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
        #f)))

(define (test-mail-rspamc file)
  "see if the mail will be filtered by rspamc. returns the box to filter to(rspamd), or #f"
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
          "rspamd")
        #f)))

(define (process-mail file rspamc)
  "process a mail. ask sievec first, and if sievec doesnt want to move the mail, then ask rspamc.
there are three possibilities, 1) sieve mailbox, 2) rspamd mailbox, or 3) no action"
  (let ((destination (or (test-mail file) (and rspamc (test-mail-rspamc file))))) 
    ;;check (access? destination)
    (progress-report)
    (if destination
        (let* ( (destination-path (format #f "~a/.~a/cur/" *maildir* destination))
                (access-path (access? destination-path W_OK)))
          (if (not access-path)
              (begin
                (format #t "folder not found, creating")
                (drysystem (format  #f "mkdir -p ~s\n"  destination-path))
                )
              )
          (format #t "\n")
          (format #t "~amv ~s  ~s\n" (if access-path "" "#") file destination-path)
          ;;TODO the "mv" might fail
          (if (not (equal?   0 (drysystem (format  #f "~amv ~s  ~s\n" "" file destination-path))))
              (exit 1))
          (incatom *msgmoved*)))))
