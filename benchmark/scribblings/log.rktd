;; This file was created by make-log-based-eval
((require benchmark plot/pict racket racket/runtime-path compiler/find-exe)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-runtime-path fib-path "examples/macro-examples/fib.rkt")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-runtime-path collatz-path "examples/macro-examples/collatz.rkt")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-runtime-path compiled-dir "examples/macro-examples/compiled")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define results
   (run-benchmarks
    (list fib-path collatz-path)
    (list (list 'jit 'no-jit))
    (lambda (file jit)
      (if (equal? jit 'jit)
        (system* (find-exe) file)
        (system* (find-exe) "-j" file)))
    #:build
    (lambda (file jit) (system* (find-exe) "-l" "raco" "make" file))
    #:clean
    (lambda (file jit)
      (system* (find-executable-path "rm") "-r" "-f" compiled-dir))
    #:num-trials
    30
    #:make-name
    (lambda (path)
      (let-values (((_1 file-name _2) (split-path path)))
        (path->string file-name)))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((parameterize
  ((plot-x-ticks no-ticks))
  (plot-pict
   #:title
   "jit vs no jit"
   #:x-label
   #f
   #:y-label
   "normalized time"
   (render-benchmark-alts (list 'jit) results)))
 ((3)
  1
  (((lib "pict/private/pict.rkt") . pict-deserialize-info))
  27
  ((q 255 255 255 1.0)
   (q (0 0 0 1.0) 1 solid round round #f)
   (q (255 255 255 1.0) solid #f #f #f)
   (q 11 #f roman normal normal #f default #f)
   (q (0 0 0 1.0) 1/2 solid round round #f)
   (q 255 255 255 1.0)
   "normalized time"
   (q 0 0 0 1.0)
   "fib.rkt"
   "collatz.rkt"
   "0"
   "5"
   "10"
   "15"
   "20"
   (q (0 2 123 1.0) 1 solid round round #f)
   (q (0 0 0 1.0) 1 transparent round round #f)
   (q 255 255 255 1.0)
   "jit"
   (q 0 0 0 1.0)
   (q (250 128 114 1.0) solid #f #f #f)
   (q ((41.0 . 23.0) (113.0 . 23.0) (113.0 . 54.625) (41.0 . 54.625)))
   (q (0 0 0 1.0) transparent #f #f #f)
   "no-jit"
   (q (0 0 139 1.0) solid #f #f #f)
   (q 12 #f default normal normal #f default #f)
   (q 0 0 0 1.0))
  ()
  (c
   values
   c
   (0
    (c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     c
     (q set-font (12 #f default normal normal #f default #f))
     c
     (q set-smoothing unsmoothed)
     c
     (q set-text-mode transparent)
     c
     (q set-alpha 1.0)
     c
     (q set-clipping-region #f)
     c
     (q set-background (255 255 255 1.0))
     c
     (q set-text-background (255 255 255 1.0))
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q set-origin 0.0 0.0)
     c
     (q set-smoothing smoothed)
     c
     (q set-text-mode transparent)
     c
     (q
      set-clipping-region
      (#t
       (((((0.0 . 0.0) (400.0 . 0.0) (400.0 . 400.0) (0.0 . 400.0)))) . any)))
     c
     (q set-font (11 #f roman normal normal #f default #f))
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     c
     (q set-background (255 255 255 1.0))
     c
     (q set-alpha 1)
     c
     (q set-origin 0.0 0.0)
     c
     (q set-smoothing smoothed)
     c
     (q set-text-mode transparent)
     c
     (q
      set-clipping-region
      (#t
       (((((0.0 . 0.0) (400.0 . 0.0) (400.0 . 400.0) (0.0 . 400.0)))) . any)))
     c
     (q set-font (11 #f roman normal normal #f default #f))
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     c
     (q set-background (255 255 255 1.0))
     c
     (q set-alpha 1)
     c
     (q set-alpha 1)
     c
     (q clear)
     c
     (q set-alpha 1)
     c
     (q draw-text "jit vs no jit" 176.5 0 #t 0 0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 34.0 381.0 394.0 381.0)
     c
     (q draw-line 34.0 16.0 394.0 16.0)
     c
     (q draw-line 34.0 16.0 34.0 381.0)
     c
     (q draw-line 394.0 16.0 394.0 381.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 106.0 376.0 106.0 386.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 322.0 376.0 322.0 386.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 29.0 381.0 39.0 381.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 31.5 339.69168071456295 36.5 339.69168071456295)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 29.0 298.3833614291259 39.0 298.3833614291259)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 31.5 257.07504214368885 36.5 257.07504214368885)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 29.0 215.76672285825182 39.0 215.76672285825182)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 31.5 174.45840357281477 36.5 174.45840357281477)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 29.0 133.15008428737772 39.0 133.15008428737772)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 31.5 91.84176500194064 36.5 91.84176500194064)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 29.0 50.53344571650365 39.0 50.53344571650365)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 381.0 399.0 381.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 339.69168071456295 396.5 339.69168071456295)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 298.3833614291259 399.0 298.3833614291259)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 257.07504214368885 396.5 257.07504214368885)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 215.76672285825182 399.0 215.76672285825182)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 174.45840357281477 396.5 174.45840357281477)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 133.15008428737772 399.0 133.15008428737772)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 91.84176500194064 396.5 91.84176500194064)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 50.53344571650365 399.0 50.53344571650365)
     c
     (q set-text-foreground (255 255 255 1.0))
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -1.5000000000000018
      233.5
      #t
      0
      1.5707963267948966)
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -1.5000000000000018
      234.5
      #t
      0
      1.5707963267948966)
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -1.5000000000000018
      235.5
      #t
      0
      1.5707963267948966)
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -0.5000000000000018
      233.5
      #t
      0
      1.5707963267948966)
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -0.5000000000000018
      235.5
      #t
      0
      1.5707963267948966)
     c
     (c draw-text c (? . 6) q 0.4999999999999982 233.5 #t 0 1.5707963267948966)
     c
     (c draw-text c (? . 6) q 0.4999999999999982 234.5 #t 0 1.5707963267948966)
     c
     (c draw-text c (? . 6) q 0.4999999999999982 235.5 #t 0 1.5707963267948966)
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -0.5000000000000018
      234.5
      #t
      0
      1.5707963267948966)
     c
     (q set-text-foreground (255 255 255 1.0))
     c
     (q draw-text "fib.rkt" 91.5 387.0 #t 0 0)
     c
     (q draw-text "fib.rkt" 91.5 388.0 #t 0 0)
     c
     (q draw-text "fib.rkt" 91.5 389.0 #t 0 0)
     c
     (q draw-text "fib.rkt" 92.5 387.0 #t 0 0)
     c
     (q draw-text "fib.rkt" 92.5 389.0 #t 0 0)
     c
     (q draw-text "fib.rkt" 93.5 387.0 #t 0 0)
     c
     (q draw-text "fib.rkt" 93.5 388.0 #t 0 0)
     c
     (q draw-text "fib.rkt" 93.5 389.0 #t 0 0)
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q draw-text "fib.rkt" 92.5 388.0 #t 0 0)
     c
     (q set-text-foreground (255 255 255 1.0))
     c
     (q draw-text "collatz.rkt" 299.0 387.0 #t 0 0)
     c
     (q draw-text "collatz.rkt" 299.0 388.0 #t 0 0)
     c
     (q draw-text "collatz.rkt" 299.0 389.0 #t 0 0)
     c
     (q draw-text "collatz.rkt" 300.0 387.0 #t 0 0)
     c
     (q draw-text "collatz.rkt" 300.0 389.0 #t 0 0)
     c
     (q draw-text "collatz.rkt" 301.0 387.0 #t 0 0)
     c
     (q draw-text "collatz.rkt" 301.0 388.0 #t 0 0)
     c
     (q draw-text "collatz.rkt" 301.0 389.0 #t 0 0)
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q draw-text "collatz.rkt" 300.0 388.0 #t 0 0)
     c
     (q set-text-foreground (255 255 255 1.0))
     c
     (c draw-text c (? . 10) q 20.0 374.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 20.0 375.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 20.0 376.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 21.0 374.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 21.0 376.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 22.0 374.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 22.0 375.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 22.0 376.5 #t 0 0)
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (c draw-text c (? . 10) q 21.0 375.5 #t 0 0)
     q
     (set-text-foreground (255 255 255 1.0))
     (draw-text "5" 20.0 291.8833614291259 #t 0 0)
     (draw-text "5" 20.0 292.8833614291259 #t 0 0)
     (draw-text "5" 20.0 293.8833614291259 #t 0 0)
     (draw-text "5" 21.0 291.8833614291259 #t 0 0)
     (draw-text "5" 21.0 293.8833614291259 #t 0 0)
     (draw-text "5" 22.0 291.8833614291259 #t 0 0)
     (draw-text "5" 22.0 292.8833614291259 #t 0 0)
     (draw-text "5" 22.0 293.8833614291259 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "5" 21.0 292.8833614291259 #t 0 0)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "10" 15.0 209.26672285825182 #t 0 0)
     (draw-text "10" 15.0 210.26672285825182 #t 0 0)
     (draw-text "10" 15.0 211.26672285825182 #t 0 0)
     (draw-text "10" 16.0 209.26672285825182 #t 0 0)
     (draw-text "10" 16.0 211.26672285825182 #t 0 0)
     (draw-text "10" 17.0 209.26672285825182 #t 0 0)
     (draw-text "10" 17.0 210.26672285825182 #t 0 0)
     (draw-text "10" 17.0 211.26672285825182 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "10" 16.0 210.26672285825182 #t 0 0)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "15" 15.0 126.65008428737772 #t 0 0)
     (draw-text "15" 15.0 127.65008428737772 #t 0 0)
     (draw-text "15" 15.0 128.65008428737772 #t 0 0)
     (draw-text "15" 16.0 126.65008428737772 #t 0 0)
     (draw-text "15" 16.0 128.65008428737772 #t 0 0)
     (draw-text "15" 17.0 126.65008428737772 #t 0 0)
     (draw-text "15" 17.0 127.65008428737772 #t 0 0)
     (draw-text "15" 17.0 128.65008428737772 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "15" 16.0 127.65008428737772 #t 0 0)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "20" 15.0 44.03344571650365 #t 0 0)
     (draw-text "20" 15.0 45.03344571650365 #t 0 0)
     (draw-text "20" 15.0 46.03344571650365 #t 0 0)
     (draw-text "20" 16.0 44.03344571650365 #t 0 0)
     (draw-text "20" 16.0 46.03344571650365 #t 0 0)
     (draw-text "20" 17.0 44.03344571650365 #t 0 0)
     (draw-text "20" 17.0 45.03344571650365 #t 0 0)
     (draw-text "20" 17.0 46.03344571650365 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "20" 16.0 45.03344571650365 #t 0 0)
     (set-clipping-region
      (#t
       (((((33.5 . 15.5) (395.0 . 15.5) (395.0 . 382.0) (33.5 . 382.0))))
        .
        any)))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (do-set-brush! ((250 128 114 1.0) solid #f #f #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((38.5 . 381.0)
       (101.5 . 381.0)
       (101.5 . 364.4766722858252)
       (38.5 . 364.4766722858252))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((38.5 . 364.4766722858252)
       (38.5 . 381.0)
       (101.5 . 381.0)
       (101.5 . 364.4766722858252)
       (38.5 . 364.4766722858252))
      0.0
      0.0)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((254.50000000000003 . 381.0)
       (317.5 . 381.0)
       (317.5 . 364.4766722858252)
       (254.50000000000003 . 364.4766722858252))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((254.50000000000003 . 364.4766722858252)
       (254.50000000000003 . 381.0)
       (317.5 . 381.0)
       (317.5 . 364.4766722858252)
       (254.50000000000003 . 364.4766722858252))
      0.0
      0.0)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line 70.0 363.1245161055847 70.0 365.8288284660657)
     (draw-line 67.0 363.1245161055847 73.0 363.1245161055847)
     (draw-line 67.0 365.8288284660657 73.0 365.8288284660657)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line 286.0 362.68188260035146 286.0 366.2714619712989)
     (draw-line 283.0 362.68188260035146 289.0 362.68188260035146)
     (draw-line 283.0 366.2714619712989 289.0 366.2714619712989)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (do-set-brush! ((0 0 139 1.0) solid #f #f #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((110.5 . 381.0)
       (173.5 . 381.0)
       (173.5 . 23.38797875750214)
       (110.5 . 23.38797875750214))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((110.5 . 23.38797875750214)
       (110.5 . 381.0)
       (173.5 . 381.0)
       (173.5 . 23.38797875750214)
       (110.5 . 23.38797875750214))
      0.0
      0.0)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((326.5 . 381.0)
       (389.5 . 381.0)
       (389.5 . 319.54960767455646)
       (326.5 . 319.54960767455646))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((326.5 . 319.54960767455646)
       (326.5 . 381.0)
       (389.5 . 381.0)
       (389.5 . 319.54960767455646)
       (326.5 . 319.54960767455646))
      0.0
      0.0)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line 142.0 16.0 142.0 30.775957515004336)
     (draw-line 139.0 16.0 145.0 16.0)
     (draw-line 139.0 30.775957515004336 145.0 30.775957515004336)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line 358.0 317.73431300088185 358.0 321.3649023482311)
     (draw-line 355.0 317.73431300088185 361.0 317.73431300088185)
     (draw-line 355.0 321.3649023482311 361.0 321.3649023482311)
     (set-origin 0.0 0.0)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-clipping-region
      (#t
       (((((0.0 . 0.0) (400.0 . 0.0) (400.0 . 400.0) (0.0 . 400.0)))) . any)))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-alpha 2/3)
     (set-smoothing unsmoothed)
     (draw-polygon
      ((41.0 . 23.0) (41.0 . 54.625) (113.0 . 54.625) (113.0 . 23.0))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (do-set-brush! ((0 0 0 1.0) transparent #f #f #f))
     (set-alpha 3/4)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((41.0 . 23.0) (41.0 . 54.625) (113.0 . 54.625) (113.0 . 23.0))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (draw-lines
      ((113.0 . 23.0)
       (41.0 . 23.0)
       (41.0 . 54.625)
       (113.0 . 54.625)
       (113.0 . 23.0))
      0.0
      0.0)
     (set-alpha 1)
     (set-clipping-region
      (#t
       (((((41.0 . 23.0) (113.0 . 23.0) (113.0 . 54.625) (41.0 . 54.625))))
        .
        any)))
     (set-text-foreground (255 255 255 1.0))
     (draw-text "jit" 43.0 24.75 #t 0 0)
     (draw-text "jit" 43.0 25.75 #t 0 0)
     (draw-text "jit" 43.0 26.75 #t 0 0)
     (draw-text "jit" 44.0 24.75 #t 0 0)
     (draw-text "jit" 44.0 26.75 #t 0 0)
     (draw-text "jit" 45.0 24.75 #t 0 0)
     (draw-text "jit" 45.0 25.75 #t 0 0)
     (draw-text "jit" 45.0 26.75 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "jit" 44.0 25.75 #t 0 0)
     (set-origin 77.0 27.125)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (do-set-brush! ((250 128 114 1.0) solid #f #f #f))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon ((0 . 0) (0 . 33/4) (33 . 33/4) (33 . 0)) 0 0 winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((33.0 . 0.0) (0.0 . 0.0) (0.0 . 8.25) (33.0 . 8.25) (33.0 . 0.0))
      0.0
      0.0)
     (set-origin 0.0 0.0)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-clipping-region
      (#t
       (((((41.0 . 23.0) (113.0 . 23.0) (113.0 . 54.625) (41.0 . 54.625))))
        .
        any)))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (do-set-brush! ((0 0 0 1.0) transparent #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "no-jit" 43.0 38.5 #t 0 0)
     (draw-text "no-jit" 43.0 39.5 #t 0 0)
     (draw-text "no-jit" 43.0 40.5 #t 0 0)
     (draw-text "no-jit" 44.0 38.5 #t 0 0)
     (draw-text "no-jit" 44.0 40.5 #t 0 0)
     (draw-text "no-jit" 45.0 38.5 #t 0 0)
     (draw-text "no-jit" 45.0 39.5 #t 0 0)
     (draw-text "no-jit" 45.0 40.5 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "no-jit" 44.0 39.5 #t 0 0)
     (set-origin 77.0 40.875)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (do-set-brush! ((0 0 139 1.0) solid #f #f #f))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon ((0 . 0) (0 . 33/4) (33 . 33/4) (33 . 0)) 0 0 winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((33.0 . 0.0) (0.0 . 0.0) (0.0 . 8.25) (33.0 . 8.25) (33.0 . 0.0))
      0.0
      0.0)
     (set-origin 0.0 0.0)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-clipping-region
      (#t
       (((((41.0 . 23.0) (113.0 . 23.0) (113.0 . 54.625) (41.0 . 54.625))))
        .
        any)))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (do-set-brush! ((0 0 0 1.0) transparent #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (set-clipping-region #f)
     (set-origin 0.0 0.0)
     (set-smoothing unsmoothed)
     (set-text-mode transparent)
     (set-clipping-region #f)
     (set-font (12 #f default normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1.0))
    400
    400
    400
    0)))
 #""
 #"")
((require benchmark plot/pict racket/match racket/vector racket/list)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define sizes (list 50000 100000))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define lists (map (lambda (i) (range i)) sizes))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define vectors (map list->vector lists))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define results
   (run-benchmarks
    (list 'square-map 'self-append)
    (list (map cons (range (length sizes)) sizes) (list 'vector 'list))
    (lambda (op index/size impl)
      (let ((fn
             (match
              (cons op impl)
              ((cons 'square-map 'vector)
               (lambda (i) (vector-map (lambda (x) (* x x)) i)))
              ((cons 'square-map 'list)
               (lambda (i) (map (lambda (x) (* x x)) i)))
              ((cons 'self-append 'vector) (lambda (i) (vector-append i i)))
              ((cons 'self-append 'list) (lambda (i) (append i i)))))
            (input
             (list-ref
              (match impl ('vector vectors) ('list lists))
              (car index/size))))
        (fn input)))
    #:extract-time
    'delta-time
    #:num-trials
    30))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((parameterize
  ((plot-x-ticks no-ticks))
  (plot-pict
   #:title
   "vectors vs lists"
   #:x-label
   #f
   #:y-label
   "normalized time"
   (render-benchmark-alts
    (list (cons 0 50000) 'list)
    results
    #:format-opts
    (lambda (opts)
      (let ((index/size (car opts)) (impl (cadr opts)))
        (format "size: ~a, ~a" (cdr index/size) impl))))))
 ((3)
  1
  (((lib "pict/private/pict.rkt") . pict-deserialize-info))
  32
  ((q 255 255 255 1.0)
   (q (0 0 0 1.0) 1 solid round round #f)
   (q (255 255 255 1.0) solid #f #f #f)
   (q 11 #f roman normal normal #f default #f)
   (q (0 0 0 1.0) 1/2 solid round round #f)
   (q 255 255 255 1.0)
   "normalized time"
   (q 0 0 0 1.0)
   "square-map"
   "self-append"
   "0"
   "1"
   "2"
   "3"
   "4"
   "5"
   (q (0 2 123 1.0) 1 solid round round #f)
   (q (0 0 0 1.0) 1 transparent round round #f)
   (q 255 255 255 1.0)
   "size: 50000, vector"
   (q 0 0 0 1.0)
   (q (250 128 114 1.0) solid #f #f #f)
   (q ((37.0 . 27.0) (174.0 . 27.0) (174.0 . 86.125) (37.0 . 86.125)))
   (q (0 0 0 1.0) transparent #f #f #f)
   "size: 50000, list"
   (q (0 0 139 1.0) solid #f #f #f)
   "size: 100000, vector"
   (q (154 205 50 1.0) solid #f #f #f)
   "size: 100000, list"
   (q (105 105 105 1.0) solid #f #f #f)
   (q 12 #f default normal normal #f default #f)
   (q 0 0 0 1.0))
  ()
  (c
   values
   c
   (0
    (c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     c
     (q set-font (12 #f default normal normal #f default #f))
     c
     (q set-smoothing unsmoothed)
     c
     (q set-text-mode transparent)
     c
     (q set-alpha 1.0)
     c
     (q set-clipping-region #f)
     c
     (q set-background (255 255 255 1.0))
     c
     (q set-text-background (255 255 255 1.0))
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q set-origin 0.0 0.0)
     c
     (q set-smoothing smoothed)
     c
     (q set-text-mode transparent)
     c
     (q
      set-clipping-region
      (#t
       (((((0.0 . 0.0) (400.0 . 0.0) (400.0 . 400.0) (0.0 . 400.0)))) . any)))
     c
     (q set-font (11 #f roman normal normal #f default #f))
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     c
     (q set-background (255 255 255 1.0))
     c
     (q set-alpha 1)
     c
     (q set-origin 0.0 0.0)
     c
     (q set-smoothing smoothed)
     c
     (q set-text-mode transparent)
     c
     (q
      set-clipping-region
      (#t
       (((((0.0 . 0.0) (400.0 . 0.0) (400.0 . 400.0) (0.0 . 400.0)))) . any)))
     c
     (q set-font (11 #f roman normal normal #f default #f))
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     c
     (q set-background (255 255 255 1.0))
     c
     (q set-alpha 1)
     c
     (q set-alpha 1)
     c
     (q clear)
     c
     (q set-alpha 1)
     c
     (q draw-text "vectors vs lists" 167.5 0 #t 0 0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 30.0 381.0 394.0 381.0)
     c
     (q draw-line 30.0 20.0 394.0 20.0)
     c
     (q draw-line 30.0 20.0 30.0 381.0)
     c
     (q draw-line 394.0 20.0 394.0 381.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 110.88888888888889 376.0 110.88888888888889 386.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 313.11111111111114 376.0 313.11111111111114 386.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 25.0 381.0 35.0 381.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 27.5 345.0898838707087 32.5 345.0898838707087)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 25.0 309.17976774141744 35.0 309.17976774141744)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 27.5 273.2696516121261 32.5 273.2696516121261)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 25.0 237.35953548283481 35.0 237.35953548283481)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 27.5 201.4494193535435 32.5 201.4494193535435)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 25.0 165.53930322425222 35.0 165.53930322425222)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 27.5 129.62918709496088 32.5 129.62918709496088)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 25.0 93.71907096566963 35.0 93.71907096566963)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 27.5 57.80895483637829 32.5 57.80895483637829)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 25.0 21.89883870708701 35.0 21.89883870708701)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 381.0 399.0 381.0)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 345.0898838707087 396.5 345.0898838707087)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 309.17976774141744 399.0 309.17976774141744)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 273.2696516121261 396.5 273.2696516121261)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 237.35953548283481 399.0 237.35953548283481)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 201.4494193535435 396.5 201.4494193535435)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 165.53930322425222 399.0 165.53930322425222)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 129.62918709496088 396.5 129.62918709496088)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 93.71907096566963 399.0 93.71907096566963)
     c
     (q do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     c
     (q draw-line 391.5 57.80895483637829 396.5 57.80895483637829)
     c
     (q do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     c
     (q draw-line 389.0 21.89883870708701 399.0 21.89883870708701)
     c
     (q set-text-foreground (255 255 255 1.0))
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -0.5000000000000018
      235.5
      #t
      0
      1.5707963267948966)
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -0.5000000000000018
      236.5
      #t
      0
      1.5707963267948966)
     c
     (c
      draw-text
      c
      (? . 6)
      q
      -0.5000000000000018
      237.5
      #t
      0
      1.5707963267948966)
     c
     (c draw-text c (? . 6) q 0.4999999999999982 235.5 #t 0 1.5707963267948966)
     c
     (c draw-text c (? . 6) q 0.4999999999999982 237.5 #t 0 1.5707963267948966)
     c
     (c draw-text c (? . 6) q 1.4999999999999982 235.5 #t 0 1.5707963267948966)
     c
     (c draw-text c (? . 6) q 1.4999999999999982 236.5 #t 0 1.5707963267948966)
     c
     (c draw-text c (? . 6) q 1.4999999999999982 237.5 #t 0 1.5707963267948966)
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (c draw-text c (? . 6) q 0.4999999999999982 236.5 #t 0 1.5707963267948966)
     c
     (q set-text-foreground (255 255 255 1.0))
     c
     (q draw-text "square-map" 84.38888888888889 387.0 #t 0 0)
     c
     (q draw-text "square-map" 84.38888888888889 388.0 #t 0 0)
     c
     (q draw-text "square-map" 84.38888888888889 389.0 #t 0 0)
     c
     (q draw-text "square-map" 85.38888888888889 387.0 #t 0 0)
     c
     (q draw-text "square-map" 85.38888888888889 389.0 #t 0 0)
     c
     (q draw-text "square-map" 86.38888888888889 387.0 #t 0 0)
     c
     (q draw-text "square-map" 86.38888888888889 388.0 #t 0 0)
     c
     (q draw-text "square-map" 86.38888888888889 389.0 #t 0 0)
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q draw-text "square-map" 85.38888888888889 388.0 #t 0 0)
     c
     (q set-text-foreground (255 255 255 1.0))
     c
     (q draw-text "self-append" 286.61111111111114 387.0 #t 0 0)
     c
     (q draw-text "self-append" 286.61111111111114 388.0 #t 0 0)
     c
     (q draw-text "self-append" 286.61111111111114 389.0 #t 0 0)
     c
     (q draw-text "self-append" 287.61111111111114 387.0 #t 0 0)
     c
     (q draw-text "self-append" 287.61111111111114 389.0 #t 0 0)
     c
     (q draw-text "self-append" 288.61111111111114 387.0 #t 0 0)
     c
     (q draw-text "self-append" 288.61111111111114 388.0 #t 0 0)
     c
     (q draw-text "self-append" 288.61111111111114 389.0 #t 0 0)
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (q draw-text "self-append" 287.61111111111114 388.0 #t 0 0)
     c
     (q set-text-foreground (255 255 255 1.0))
     c
     (c draw-text c (? . 10) q 16.0 374.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 16.0 375.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 16.0 376.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 17.0 374.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 17.0 376.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 18.0 374.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 18.0 375.5 #t 0 0)
     c
     (c draw-text c (? . 10) q 18.0 376.5 #t 0 0)
     c
     (q set-text-foreground (0 0 0 1.0))
     c
     (c draw-text c (? . 10) q 17.0 375.5 #t 0 0)
     q
     (set-text-foreground (255 255 255 1.0))
     (draw-text "1" 16.0 302.67976774141744 #t 0 0)
     (draw-text "1" 16.0 303.67976774141744 #t 0 0)
     (draw-text "1" 16.0 304.67976774141744 #t 0 0)
     (draw-text "1" 17.0 302.67976774141744 #t 0 0)
     (draw-text "1" 17.0 304.67976774141744 #t 0 0)
     (draw-text "1" 18.0 302.67976774141744 #t 0 0)
     (draw-text "1" 18.0 303.67976774141744 #t 0 0)
     (draw-text "1" 18.0 304.67976774141744 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "1" 17.0 303.67976774141744 #t 0 0)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "2" 16.0 230.85953548283481 #t 0 0)
     (draw-text "2" 16.0 231.85953548283481 #t 0 0)
     (draw-text "2" 16.0 232.85953548283481 #t 0 0)
     (draw-text "2" 17.0 230.85953548283481 #t 0 0)
     (draw-text "2" 17.0 232.85953548283481 #t 0 0)
     (draw-text "2" 18.0 230.85953548283481 #t 0 0)
     (draw-text "2" 18.0 231.85953548283481 #t 0 0)
     (draw-text "2" 18.0 232.85953548283481 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "2" 17.0 231.85953548283481 #t 0 0)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "3" 16.0 159.03930322425222 #t 0 0)
     (draw-text "3" 16.0 160.03930322425222 #t 0 0)
     (draw-text "3" 16.0 161.03930322425222 #t 0 0)
     (draw-text "3" 17.0 159.03930322425222 #t 0 0)
     (draw-text "3" 17.0 161.03930322425222 #t 0 0)
     (draw-text "3" 18.0 159.03930322425222 #t 0 0)
     (draw-text "3" 18.0 160.03930322425222 #t 0 0)
     (draw-text "3" 18.0 161.03930322425222 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "3" 17.0 160.03930322425222 #t 0 0)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "4" 16.0 87.21907096566963 #t 0 0)
     (draw-text "4" 16.0 88.21907096566963 #t 0 0)
     (draw-text "4" 16.0 89.21907096566963 #t 0 0)
     (draw-text "4" 17.0 87.21907096566963 #t 0 0)
     (draw-text "4" 17.0 89.21907096566963 #t 0 0)
     (draw-text "4" 18.0 87.21907096566963 #t 0 0)
     (draw-text "4" 18.0 88.21907096566963 #t 0 0)
     (draw-text "4" 18.0 89.21907096566963 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "4" 17.0 88.21907096566963 #t 0 0)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "5" 16.0 15.398838707087009 #t 0 0)
     (draw-text "5" 16.0 16.39883870708701 #t 0 0)
     (draw-text "5" 16.0 17.39883870708701 #t 0 0)
     (draw-text "5" 17.0 15.398838707087009 #t 0 0)
     (draw-text "5" 17.0 17.39883870708701 #t 0 0)
     (draw-text "5" 18.0 15.398838707087009 #t 0 0)
     (draw-text "5" 18.0 16.39883870708701 #t 0 0)
     (draw-text "5" 18.0 17.39883870708701 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "5" 17.0 16.39883870708701 #t 0 0)
     (set-clipping-region
      (#t
       (((((29.5 . 19.5) (395.0 . 19.5) (395.0 . 382.0) (29.5 . 382.0))))
        .
        any)))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (do-set-brush! ((250 128 114 1.0) solid #f #f #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((32.52777777777778 . 381.0)
       (67.91666666666667 . 381.0)
       (67.91666666666667 . 188.2981222430655)
       (32.52777777777778 . 188.2981222430655))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((32.52777777777778 . 188.2981222430655)
       (32.52777777777778 . 381.0)
       (67.91666666666667 . 381.0)
       (67.91666666666667 . 188.2981222430655)
       (32.52777777777778 . 188.2981222430655))
      0.0
      0.0)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((234.75 . 381.0)
       (270.1388888888889 . 381.0)
       (270.1388888888889 . 308.3319316508121)
       (234.75 . 308.3319316508121))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((234.75 . 308.3319316508121)
       (234.75 . 381.0)
       (270.1388888888889 . 381.0)
       (270.1388888888889 . 308.3319316508121)
       (234.75 . 308.3319316508121))
      0.0
      0.0)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line
      50.22222222222222
      146.65595699806516
      50.22222222222222
      229.94028748806588)
     (draw-line
      47.22222222222222
      146.65595699806516
      53.22222222222222
      146.65595699806516)
     (draw-line
      47.22222222222222
      229.94028748806588
      53.22222222222222
      229.94028748806588)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line
      252.44444444444446
      302.26490647984144
      252.44444444444446
      314.3989568217828)
     (draw-line
      249.44444444444446
      302.26490647984144
      255.44444444444446
      302.26490647984144)
     (draw-line
      249.44444444444446
      314.3989568217828
      255.44444444444446
      314.3989568217828)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (do-set-brush! ((0 0 139 1.0) solid #f #f #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((72.97222222222223 . 381.0)
       (108.36111111111111 . 381.0)
       (108.36111111111111 . 309.17976774141744)
       (72.97222222222223 . 309.17976774141744))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((72.97222222222223 . 309.17976774141744)
       (72.97222222222223 . 381.0)
       (108.36111111111111 . 381.0)
       (108.36111111111111 . 309.17976774141744)
       (72.97222222222223 . 309.17976774141744))
      0.0
      0.0)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((275.19444444444446 . 381.0)
       (310.58333333333337 . 381.0)
       (310.58333333333337 . 309.17976774141744)
       (275.19444444444446 . 309.17976774141744))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((275.19444444444446 . 309.17976774141744)
       (275.19444444444446 . 381.0)
       (310.58333333333337 . 381.0)
       (310.58333333333337 . 309.17976774141744)
       (275.19444444444446 . 309.17976774141744))
      0.0
      0.0)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line
      90.66666666666666
      285.29127377090043
      90.66666666666666
      333.0682617119344)
     (draw-line
      87.66666666666666
      285.29127377090043
      93.66666666666666
      285.29127377090043)
     (draw-line
      87.66666666666666
      333.0682617119344
      93.66666666666666
      333.0682617119344)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line
      292.88888888888886
      277.453769618455
      292.88888888888886
      340.9057658643798)
     (draw-line
      289.88888888888886
      277.453769618455
      295.88888888888886
      277.453769618455)
     (draw-line
      289.88888888888886
      340.9057658643798
      295.88888888888886
      340.9057658643798)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (do-set-brush! ((154 205 50 1.0) solid #f #f #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((113.41666666666666 . 381.0)
       (148.80555555555554 . 381.0)
       (148.80555555555554 . 42.35581519395953)
       (113.41666666666666 . 42.35581519395953))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((113.41666666666666 . 42.35581519395953)
       (113.41666666666666 . 381.0)
       (148.80555555555554 . 381.0)
       (148.80555555555554 . 42.35581519395953)
       (113.41666666666666 . 42.35581519395953))
      0.0
      0.0)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((315.63888888888886 . 381.0)
       (351.02777777777777 . 381.0)
       (351.02777777777777 . 227.5720267967034)
       (315.63888888888886 . 227.5720267967034))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((315.63888888888886 . 227.5720267967034)
       (315.63888888888886 . 381.0)
       (351.02777777777777 . 381.0)
       (351.02777777777777 . 227.5720267967034)
       (315.63888888888886 . 227.5720267967034))
      0.0
      0.0)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line 131.11111111111111 20.0 131.11111111111111 64.711630387919)
     (draw-line 128.11111111111111 20.0 134.11111111111111 20.0)
     (draw-line
      128.11111111111111
      64.711630387919
      134.11111111111111
      64.711630387919)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line
      333.33333333333337
      206.12427885945064
      333.33333333333337
      249.01977473395613)
     (draw-line
      330.33333333333337
      206.12427885945064
      336.33333333333337
      206.12427885945064)
     (draw-line
      330.33333333333337
      249.01977473395613
      336.33333333333337
      249.01977473395613)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (do-set-brush! ((105 105 105 1.0) solid #f #f #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((153.86111111111111 . 381.0)
       (189.25 . 381.0)
       (189.25 . 208.99083318679993)
       (153.86111111111111 . 208.99083318679993))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((153.86111111111111 . 208.99083318679993)
       (153.86111111111111 . 381.0)
       (189.25 . 381.0)
       (189.25 . 208.99083318679993)
       (153.86111111111111 . 208.99083318679993))
      0.0
      0.0)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((356.08333333333337 . 381.0)
       (391.47222222222223 . 381.0)
       (391.47222222222223 . 267.3892233906922)
       (356.08333333333337 . 267.3892233906922))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((356.08333333333337 . 267.3892233906922)
       (356.08333333333337 . 381.0)
       (391.47222222222223 . 381.0)
       (391.47222222222223 . 267.3892233906922)
       (356.08333333333337 . 267.3892233906922))
      0.0
      0.0)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line
      171.55555555555557
      186.88654213681704
      171.55555555555557
      231.09512423678285)
     (draw-line
      168.55555555555557
      186.88654213681704
      174.55555555555557
      186.88654213681704)
     (draw-line
      168.55555555555557
      231.09512423678285
      174.55555555555557
      231.09512423678285)
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (set-alpha 2/3)
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (draw-line
      373.77777777777777
      236.06626784892515
      373.77777777777777
      298.7121789324593)
     (draw-line
      370.77777777777777
      236.06626784892515
      376.77777777777777
      236.06626784892515)
     (draw-line
      370.77777777777777
      298.7121789324593
      376.77777777777777
      298.7121789324593)
     (set-origin 0.0 0.0)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-clipping-region
      (#t
       (((((0.0 . 0.0) (400.0 . 0.0) (400.0 . 400.0) (0.0 . 400.0)))) . any)))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-alpha 2/3)
     (set-smoothing unsmoothed)
     (draw-polygon
      ((37.0 . 27.0) (37.0 . 86.125) (174.0 . 86.125) (174.0 . 27.0))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (do-set-brush! ((0 0 0 1.0) transparent #f #f #f))
     (set-alpha 3/4)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon
      ((37.0 . 27.0) (37.0 . 86.125) (174.0 . 86.125) (174.0 . 27.0))
      0
      0
      winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (draw-lines
      ((174.0 . 27.0)
       (37.0 . 27.0)
       (37.0 . 86.125)
       (174.0 . 86.125)
       (174.0 . 27.0))
      0.0
      0.0)
     (set-alpha 1)
     (set-clipping-region
      (#t
       (((((37.0 . 27.0) (174.0 . 27.0) (174.0 . 86.125) (37.0 . 86.125))))
        .
        any)))
     (set-text-foreground (255 255 255 1.0))
     (draw-text "size: 50000, vector" 39.0 28.75 #t 0 0)
     (draw-text "size: 50000, vector" 39.0 29.75 #t 0 0)
     (draw-text "size: 50000, vector" 39.0 30.75 #t 0 0)
     (draw-text "size: 50000, vector" 40.0 28.75 #t 0 0)
     (draw-text "size: 50000, vector" 40.0 30.75 #t 0 0)
     (draw-text "size: 50000, vector" 41.0 28.75 #t 0 0)
     (draw-text "size: 50000, vector" 41.0 29.75 #t 0 0)
     (draw-text "size: 50000, vector" 41.0 30.75 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "size: 50000, vector" 40.0 29.75 #t 0 0)
     (set-origin 138.0 31.125)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (do-set-brush! ((250 128 114 1.0) solid #f #f #f))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon ((0 . 0) (0 . 33/4) (33 . 33/4) (33 . 0)) 0 0 winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((33.0 . 0.0) (0.0 . 0.0) (0.0 . 8.25) (33.0 . 8.25) (33.0 . 0.0))
      0.0
      0.0)
     (set-origin 0.0 0.0)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-clipping-region
      (#t
       (((((37.0 . 27.0) (174.0 . 27.0) (174.0 . 86.125) (37.0 . 86.125))))
        .
        any)))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (do-set-brush! ((0 0 0 1.0) transparent #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "size: 50000, list" 39.0 42.5 #t 0 0)
     (draw-text "size: 50000, list" 39.0 43.5 #t 0 0)
     (draw-text "size: 50000, list" 39.0 44.5 #t 0 0)
     (draw-text "size: 50000, list" 40.0 42.5 #t 0 0)
     (draw-text "size: 50000, list" 40.0 44.5 #t 0 0)
     (draw-text "size: 50000, list" 41.0 42.5 #t 0 0)
     (draw-text "size: 50000, list" 41.0 43.5 #t 0 0)
     (draw-text "size: 50000, list" 41.0 44.5 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "size: 50000, list" 40.0 43.5 #t 0 0)
     (set-origin 138.0 44.875)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (do-set-brush! ((0 0 139 1.0) solid #f #f #f))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon ((0 . 0) (0 . 33/4) (33 . 33/4) (33 . 0)) 0 0 winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((33.0 . 0.0) (0.0 . 0.0) (0.0 . 8.25) (33.0 . 8.25) (33.0 . 0.0))
      0.0
      0.0)
     (set-origin 0.0 0.0)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-clipping-region
      (#t
       (((((37.0 . 27.0) (174.0 . 27.0) (174.0 . 86.125) (37.0 . 86.125))))
        .
        any)))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (do-set-brush! ((0 0 0 1.0) transparent #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "size: 100000, vector" 39.0 56.25 #t 0 0)
     (draw-text "size: 100000, vector" 39.0 57.25 #t 0 0)
     (draw-text "size: 100000, vector" 39.0 58.25 #t 0 0)
     (draw-text "size: 100000, vector" 40.0 56.25 #t 0 0)
     (draw-text "size: 100000, vector" 40.0 58.25 #t 0 0)
     (draw-text "size: 100000, vector" 41.0 56.25 #t 0 0)
     (draw-text "size: 100000, vector" 41.0 57.25 #t 0 0)
     (draw-text "size: 100000, vector" 41.0 58.25 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "size: 100000, vector" 40.0 57.25 #t 0 0)
     (set-origin 138.0 58.625)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (do-set-brush! ((154 205 50 1.0) solid #f #f #f))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon ((0 . 0) (0 . 33/4) (33 . 33/4) (33 . 0)) 0 0 winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((33.0 . 0.0) (0.0 . 0.0) (0.0 . 8.25) (33.0 . 8.25) (33.0 . 0.0))
      0.0
      0.0)
     (set-origin 0.0 0.0)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-clipping-region
      (#t
       (((((37.0 . 27.0) (174.0 . 27.0) (174.0 . 86.125) (37.0 . 86.125))))
        .
        any)))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (do-set-brush! ((0 0 0 1.0) transparent #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (set-text-foreground (255 255 255 1.0))
     (draw-text "size: 100000, list" 39.0 70.0 #t 0 0)
     (draw-text "size: 100000, list" 39.0 71.0 #t 0 0)
     (draw-text "size: 100000, list" 39.0 72.0 #t 0 0)
     (draw-text "size: 100000, list" 40.0 70.0 #t 0 0)
     (draw-text "size: 100000, list" 40.0 72.0 #t 0 0)
     (draw-text "size: 100000, list" 41.0 70.0 #t 0 0)
     (draw-text "size: 100000, list" 41.0 71.0 #t 0 0)
     (draw-text "size: 100000, list" 41.0 72.0 #t 0 0)
     (set-text-foreground (0 0 0 1.0))
     (draw-text "size: 100000, list" 40.0 71.0 #t 0 0)
     (set-origin 138.0 72.375)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (do-set-brush! ((105 105 105 1.0) solid #f #f #f))
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (set-alpha 1)
     (do-set-pen! ((0 0 0 1.0) 1 transparent round round #f))
     (set-smoothing unsmoothed)
     (draw-polygon ((0 . 0) (0 . 33/4) (33 . 33/4) (33 . 0)) 0 0 winding)
     (set-smoothing smoothed)
     (do-set-pen! ((0 2 123 1.0) 1 solid round round #f))
     (draw-lines
      ((33.0 . 0.0) (0.0 . 0.0) (0.0 . 8.25) (33.0 . 8.25) (33.0 . 0.0))
      0.0
      0.0)
     (set-origin 0.0 0.0)
     (set-smoothing smoothed)
     (set-text-mode transparent)
     (set-clipping-region
      (#t
       (((((37.0 . 27.0) (174.0 . 27.0) (174.0 . 86.125) (37.0 . 86.125))))
        .
        any)))
     (set-font (11 #f roman normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1/2 solid round round #f))
     (do-set-brush! ((0 0 0 1.0) transparent #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1)
     (set-clipping-region #f)
     (set-origin 0.0 0.0)
     (set-smoothing unsmoothed)
     (set-text-mode transparent)
     (set-clipping-region #f)
     (set-font (12 #f default normal normal #f default #f))
     (set-text-foreground (0 0 0 1.0))
     (do-set-pen! ((0 0 0 1.0) 1 solid round round #f))
     (do-set-brush! ((255 255 255 1.0) solid #f #f #f))
     (set-background (255 255 255 1.0))
     (set-alpha 1.0))
    400
    400
    400
    0)))
 #""
 #"")
