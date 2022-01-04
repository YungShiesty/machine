#lang racket

(define fsm '(
(("a" "Click_Borrow")"b")
(("a" "Click_My_Account")"c")
(("a" "Click_Renew")"d")
(("b" "ID_Scanned")"e")
(("c" "ID_Scanned")"f")
(("d" "ID_Scanned")"g")
(("e" "Available_Book")"h")
(("e" "Invalid_Object")"i")
(("e" "Click Done")"a")
(("i" "Try_Again")"e")
(("f" "Click")"j")
(("j" "Back") "f")
(("f" "Click")"k")
(("k" "Back")"f")
(("f" "Click")"l")
(("l" "Back")"f")
(("f" "Click")"m")
(("m" "Back")"f")
(("f" "Click_Borrow")"e")
(("f" "Click_Renew")"g")
(("f" "Click_Done")"a")
(("g" "Click_Books")"n")
(("g" "Click_Renewall")"o")
(("g" "Click_Done")"a")
(("n" "Click_Renew")"o")))



(define conversion '(
("a" "Homepage")
("b" "scan_id")
("c" "scan_id2")
("d" "scan_id3")
("e" "Place_Books_On_Shelf")
("f" "Account_Info")
("g" "Renew_Info")
("h" "Successfully_Borrowed")
("i" "Error")
("j" "Hold")
("k" "On_loan")
("l" "Items_Overdue")
("m" "Fined_Item")
("n" "Books_Are_Highlighted")
("o" "Renew_Confirmation")))

(define next-state (λ ( x y z)
                            (cond
                              ((empty? z) #f)
                              ((and (equal? (first(first(first z))) x) (equal? (second(first(first z))) y))(second(first z)))
                            (#t (next-state x y (rest z)))
                            (else "check inputs"))))

(define translate (λ (t p)
                    (cond
                      ((empty? p) #f)
                      ((equal? (first(first p)) t ) (second (first p)))
                    (#t (translate t (rest p))))))

(define run-sequence (λ (init-state event-seq list)
                       (cond
                         ((and (empty? event-seq) (empty? list)) "input valid event-seq and list")
                         ((empty? event-seq) "input valid event-seq")
                         ((empty? list) "input valid list")
                         ((and (string? (first event-seq)) (list? list))(next-state init-state (first event-seq) list)
                       ((run-sequence (next-state init-state (first event-seq) list) (first(rest event-seq)) list))))))
