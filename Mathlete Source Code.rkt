;; ******************************************************************************************************************************************************
;; ******************************************************************************************************************************************************
;; ******************************************************************************************************************************************************
;;            ##   ##  #######  ########  ##   ##  ##       #######  ########  #######
;;            ### ###  ##   ##     ##     ##   ##  ##       ##          ##     ##
;;            ## # ##  #######     ##     #######  ##       ####        ##     ####
;;            ##   ##  ##   ##     ##     ##   ##  ##       ##          ##     ##
;;            ##   ##  ##   ##     ##     ##   ##  #######  #######     ##     #######
;; ******************************************************************************************************************************************************
;; ******************************************************************************************************************************************************
;; ******************************************************************************************************************************************************

;; a math (algebra) game where you compete against yourself seeing how many answers you can get correct before you fail
;; 3 difficulty levels for the computer (easy medium hard)
;; function startgame will start the game and keep asking math questions until you get one wrong or don't answer in time

;; **************************************************
;; Language
#lang racket
;; **************************************************

;; **************************************************
;; Provided Function
(provide startgame)
;; **************************************************

;; **************************************************
;; Starting the game
;; **************************************************
(define (startgame)
  (printf "Please enter your name\n")
  (define name (read))
  (printf "Please enter a difficulty\n")
  (printf "You can choose from: Easy or Hard\n")
  (define skill (read))
  (cond
    [(or (equal? skill 'easy) (equal? skill 'Easy)) (introduction name 'Easy)
                                                    (sleep 3)
                                                    (restart)
                                                    (sleep 5)]
    [(or (equal? skill 'hard) (equal? skill 'Hard)) (introduction name 'Hard)
                                                    (sleep 3)
                                                    (restart)
                                                    (sleep 5)]   
    [else (printf "Sorry that isn't a skill level, please enter a valid skill level\n")
          (printf "Restarting game...\n")
          (sleep 3)
          (startgame)
          (sleep 3)
          (restart)]))
;; **************************************************

;; **************************************************
;; Restart game
;; **************************************************
(define (restart)
  (printf "Would you like to play again? (yes/no)\n")
  (define answer (read))
  (cond
    [(or (equal? answer 'Yes) (equal? answer 'yes)) (startgame)]
    [(or (equal? answer 'No) (equal? answer 'no)) (printf "Here is your highscore for this session\n")
                                                  (highscore 'Easy)
                                                  (sleep 3)
                                                  (highscore 'Hard)
                                                  ]
    [else (printf "Sorry, I didn't understand that\n")
          (restart)]))
;; **************************************************

;; **************************************************
;; Introduction
;; **************************************************
(define (introduction name skill)
  (define (time-for-question skill)
    (cond
      [(equal? skill 'Easy) 4]
      [(equal? skill 'Hard) 6]))
  (define (time-for-question2 skill)
    (cond
      [(equal? skill 'Easy) 1]
      [(equal? skill 'Hard) 3]))
  (printf "Hi ~a! Welcome to Mathlete! Here we will test your math skills with several basic math questions!\n" name)
  (sleep 3)
  (printf "You have picked ~a\nThe rules are as follows:\n" skill)
  (sleep 3)
  (printf "1: You will have ~a seconds to answer each question\n" (time-for-question skill))
  (sleep 3)
  (printf "2: Every 10 questions you answer right, the timer will decrease by one second to a minimum of ~a second(s) per question\n" (time-for-question2 skill))
  (sleep 3)
  (printf "3: You will get 1 point for each question you get right\n")
  (sleep 3)
  (printf "4: You keep going until you get a question wrong or don't answer in time\n")
  (sleep 3)
  (printf "The game will start in 5 seconds...\n")
  (printf "Goodluck!\n")
  (sleep 5)
  (cond
    [(equal? skill 'Easy) (starteasy 1 name 4 0)]
    [(equal? skill 'Hard) (starthard 1 name 6 0)]))
;; **************************************************


;; **************************************************
;; Highscores and Information
;; **************************************************
(define-struct Information (Name Score Difficulty))
(define EasyInfo (make-Information 'Noone 0 'Easy))
(define HardInfo (make-Information 'Noone 0 'Hard))
(define (highscore difficulty)
  (cond
    [(equal? difficulty 'Easy)
     (printf "The highscore for Easy is: ~a and was achieved by ~a\n" (Information-Score EasyInfo) (Information-Name EasyInfo))]
    [(equal? difficulty 'Hard)
     (printf "The highscore for Hard is: ~a and was achieved by ~a\n" (Information-Score HardInfo) (Information-Name HardInfo))]
    [else (printf "Sorry that wasn't a valid skill level, Please enter either easy, medium or hard")]))
;; **************************************************


;; **************************************************
;; Random Effects
;; **************************************************
(define (effects n k bump)
  (define (printast n)
    (cond
      [(= n 0) (printf "*\n")]
      [else (printf "*")
            (printast (- n 1))]))
  (cond
    [(< n bump) (printast n)
                (sleep 0.02)
                (effects (add1 n) k bump)]
    [(< k bump) (printast (- n k))
                (sleep 0.02)
                (effects n (add1 k) bump)]
    [(= k bump) (printast (- n k))]))
(define (real-effects)
  (effects 0 1 50)
  (effects 0 1 30)
  (effects 0 1 10))
;; **************************************************





;; **************************************************
;; EASY
;; **************************************************
(define (starteasy score name timer scoretracker)
;; **************************************************
;; Information about the player
;; **************************************************
  (define playerinfo (make-Information name scoretracker 'Easy))
;; **************************************************
;; **************************************************
;; All the functions needed to do basic random arithmetic
;; **************************************************
  (define (randomnum)
    (define random-number (+ 1 (random 3)))
    random-number)
  (define X (randomnum))
  (define (plus a b)
    (+ a b))
  (define (minus a b)
    (- a b))
  (define (multiply a b)
    (* a b))
  (define (arithmetic)
    (define x-helper X)
    (cond
      [(equal? x-helper 1) plus]
      [(equal? x-helper 2) minus]
      [(equal? x-helper 3) multiply]))
  (define x (+ 1 (random 5)))
  (define y (+ 1 (random 5)))
  (define (z)
    (define x2 X)
    (cond
      [(equal? x2 1) "+"]
      [(equal? x2 2) "-"]
      [(equal? x2 3) "*"]))
;; **************************************************
;; Displayed answers and correct answers with timer
;; **************************************************
  (printf "~a ~a ~a\n" x (z) y)
  (define start-time (current-seconds))
  (define answer ((arithmetic) x y))
  (define useranswer (read))
  (define end-time (current-seconds))
  (sleep (min timer 0.5))
;; **************************************************
;; Conditionals for ending game
;; **************************************************
  (cond
    [(and (> score 9) (= useranswer answer) (equal? timer 1) (<= (- end-time start-time) timer)) (starteasy score name timer (+ 1 scoretracker))]
    [(and (> score 9) (= useranswer answer) (not (equal? timer 2)) (<= (- end-time start-time) timer)) (real-effects)
                                                                                                       (printf "Congrats, you have correctly answered 10 questions each in under ~a seconds\n" timer)
                                                                                                       (sleep 3)
                                                                                                       (printf "You now have ~a seconds to answer each question\n" (- timer 1))
                                                                                                       (sleep 3)
                                                                                                       (printf "The next stage will start in 5 seconds...\n")
                                                                                                       (printf "Goodluck!\n")
                                                                                                       (sleep 5)
                                                                                                       (starteasy 0 name (- timer 1) (+ 1 scoretracker))]
    [(and (> score 9) (= useranswer answer) (equal? timer 2) (<= (- end-time start-time) timer)) (real-effects)
                                                                                                 (printf "Congrats, you have correctly answered 10 questions each in under ~a seconds\n" timer)
                                                                                                 (sleep 3)
                                                                                                 (printf "You have now reached the final stage\n")
                                                                                                 (sleep 3)
                                                                                                 (printf "You will have 1 second to answer each question!\n")
                                                                                                 (sleep 3)
                                                                                                 (printf "The final stage will start in 5 seconds...\n")
                                                                                                 (printf "Goodluck!\n")
                                                                                                 (sleep 5)
                                                                                                 (starteasy 0 name (- timer 1) (+ 1 scoretracker))]
    [(and (= useranswer answer) (<= (- end-time start-time) timer)) (starteasy (+ 1 score) name timer (+ 1 scoretracker))]
    [else
     (cond
       [(and (not (<= (- end-time start-time) timer)) (> (Information-Score playerinfo) (Information-Score EasyInfo)))
        (printf "Sorry, you answered too slowly!\nYour total score is: ~a\n" scoretracker)
        (sleep 2)
        (printf "Congrats ~a! You have made the new highscore for Easy mode!\n" name)
        (set! EasyInfo (make-Information name scoretracker 'Easy))]
       [(> (Information-Score playerinfo) (Information-Score EasyInfo))
        (printf "Sorry, ~a that was incorrect, the correct answer was: ~a \nYour total score is: ~a\n" name answer scoretracker)
        (sleep 2)
        (printf "Congrats ~a! You have made the new highscore for Easy mode!\n" name)
        (set! EasyInfo (make-Information name scoretracker 'Easy))]
       [else (printf "Sorry, ~a that was incorrect, the correct answer was: ~a \nYour total score is: ~a\n" name answer scoretracker)])]))
;; **************************************************


;; **************************************************
;; HARD
;; **************************************************
(define (starthard score name timer scoretracker)
;; **************************************************
;; Information about the player
;; **************************************************
  (define playerinfo (make-Information name scoretracker 'Hard))
;; **************************************************
;; **************************************************
;; All the functions needed to do basic random arithmetic
;; **************************************************
  (define (randomnum)
    (define random-number (+ 1 (random 4)))
    random-number)
  (define X (randomnum))
  (define (plus a b)
    (+ a b))
  (define (minus a b)
    (- a b))
  (define (multiply a b)
    (* a b))
  (define (divide a b)
    (/ a b))
  (define (arithmetic)
    (define x-helper X)
    (cond
      [(equal? x-helper 1) plus]
      [(equal? x-helper 2) minus]
      [(equal? x-helper 3) multiply]
      [(equal? x-helper 4) divide]))
  (define (x)
    (define x-num (- (random 21) 10))
    (cond
      [(equal? x-num 0) (x)]
      [else x-num]))
  (define (y) 
    (define y-num (- (random 21) 10))
    (cond
      [(equal? y-num 0) (y)]
      [else y-num]))
  (define (z)
    (define x2 X)
    (cond
      [(equal? x2 1) "+"]
      [(equal? x2 2) "-"]
      [(equal? x2 3) "*"]
      [(equal? x2 4) "/"]))
  (define x-calc (x))
  (define y-calc (y))
  (define (y-use-changer)
    (define random-num (random 2))
    (cond
      [(= random-num 0) -2]
      [(= random-num 1) 2]))
  (define y-constant (y-use-changer))
;; **************************************************
;; Function to make sure non-integer division doesn't occur
  (define (non-integer-division answer1)
    (cond
      [(integer? answer1) #t]
      [(and (integer? (- answer1 0.5)) (or (> answer1 4) (< answer1 -4))) #t]
      [else (set! y-calc y-constant)
            (set! answer (/ x-calc y-calc))]))
;; **************************************************
;; **************************************************
;; Displayed answers and correct answers with timer without odd division
;; **************************************************
  (define answer ((arithmetic) x-calc y-calc))
  (non-integer-division answer)
  (printf "(~a) ~a (~a)\n" x-calc (z) y-calc)
  (define start-time (current-seconds))
  (define useranswer (read))
  (define end-time (current-seconds))
  (sleep (min timer 0.5))
;; **************************************************
;; Conditionals for ending game
;; **************************************************
  (cond
    [(and (> score 9) (= useranswer answer) (equal? timer 5) (<= (- end-time start-time) timer)) (starthard score name timer (+ 1 scoretracker))]
    [(and (> score 9) (= useranswer answer) (not (equal? timer 6)) (<= (- end-time start-time) timer)) (real-effects)
                                                                                                       (printf "Congrats, you have correctly answered 10 questions each in under ~a seconds\n" timer)
                                                                                                       (sleep 3)
                                                                                                       (printf "You now have ~a seconds to answer each question\n" (- timer 1))
                                                                                                       (sleep 3)
                                                                                                       (printf "The next stage will start in 5 seconds...\n")
                                                                                                       (printf "Goodluck!\n")
                                                                                                       (sleep 5)
                                                                                                       (starthard 0 name (- timer 1) (+ 1 scoretracker))]
    [(and (> score 9) (= useranswer answer) (equal? timer 6) (<= (- end-time start-time) timer)) (real-effects)
                                                                                                 (printf "Congrats, you have correctly answered 10 questions each in under ~a seconds\n" timer)
                                                                                                 (sleep 3)
                                                                                                 (printf "You have now reached the final stage\n")
                                                                                                 (sleep 3)
                                                                                                 (printf "You will have 5 second to answer each question!\n")
                                                                                                 (sleep 3)
                                                                                                 (printf "The final stage will start in 5 seconds...\n")
                                                                                                 (printf "Goodluck!\n")
                                                                                                 (sleep 5)
                                                                                                 (starthard 0 name (- timer 1) (+ 1 scoretracker))]
    [(and (= useranswer answer) (<= (- end-time start-time) timer)) (starthard (+ 1 score) name timer (+ 1 scoretracker))]
    [else
     (cond
       [(and (not (<= (- end-time start-time) timer)) (> (Information-Score playerinfo) (Information-Score EasyInfo)))
        (printf "Sorry, you answered too slowly!\nYour total score is: ~a\n" scoretracker)
        (sleep 2)
        (printf "Congrats ~a! You have made the new highscore for Hard mode!\n" name)
        (set! HardInfo (make-Information name scoretracker 'Hard))]
       [(> (Information-Score playerinfo) (Information-Score EasyInfo))
        (printf "Sorry, ~a that was incorrect, the correct answer was: ~a \nYour total score is: ~a\n" name answer scoretracker)
        (sleep 2)
        (printf "Congrats ~a! You have made the new highscore for Hard mode!\n" name)
        (set! HardInfo (make-Information name scoretracker 'Hard))]
       [else (printf "Sorry, ~a that was incorrect, the correct answer was: ~a \nYour total score is: ~a\n" name answer scoretracker)])]))
;; **************************************************
(startgame)
;; ******************************************************************************************************************************************************
;; ******************************************************************************************************************************************************
;; ******************************************************************************************************************************************************
;;            ##   ##  #######  ########  ##   ##  ##       #######  ########  #######
;;            ### ###  ##   ##     ##     ##   ##  ##       ##          ##     ##
;;            ## # ##  #######     ##     #######  ##       ####        ##     ####
;;            ##   ##  ##   ##     ##     ##   ##  ##       ##          ##     ##
;;            ##   ##  ##   ##     ##     ##   ##  #######  #######     ##     #######
;; ******************************************************************************************************************************************************
;; ******************************************************************************************************************************************************
;; ******************************************************************************************************************************************************