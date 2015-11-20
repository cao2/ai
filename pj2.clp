(defclass CUSTOMER (is-a USER)
		(role concrete)
		(slot cname)
		(slot concern)
		(slot skin_type)
		(slot age (type NUMBER))
		(slot daily_environment))
(defclass PREFER (is-a USER)
		(role concrete)
		(slot concern)
		(slot price_range)
		(slot age_group)
		(slot organic)
)
	
(deffunction check-input($?allowed)
	;(printout t "Please enter your " ?attri crlf)
	;(printout t "Please choose between" ?allowed "  " crlf)
	(bind ?inp (read))
	(while( not (member ?inp ?allowed)) do
		(printout t "wrong answer, please re-enter")
		(bind ?inp (read)))
?inp)

(deffunction check-age()
	(printout t "How old are you? ")
	(bind ?inp (read))
	(while(or (<= ?inp 0) (>= ?inp 200)) do
		(printout t "invalid input, please re-eneter:  ")
		(bind ?inp (read))
	)
	?inp
)

(defrule create-cus 
=>
	(printout t "Hi there, how are you. " crlf)
	(printout t "Just answer couple questios and I can help you with your cosmtic brand choosing" crlf)
	(make-instance cus of CUSTOMER (age 50)(daily_environment regular))
	(make-instance pre of PREFER (organic yes))
)	

(defrule read-name (declare (salience 20))
?ins<-(object(is-a CUSTOMER))
=>
	(printout t "what's your name " crlf)
	(send ?ins put-cname (read))
)

(defrule read-age 
?ins<-(object(is-a CUSTOMER)(cname ?nm))
=>
	;(printout t "Hi " ?nm ", how old are you " crlf)
	(bind ?age (check-age))
	(send ?ins put-age ?age)
)

	
(defrule read-envi 
?ins<-(object(is-a CUSTOMER))
=>
	(printout t "Please enter your daily environment" crlf)
	(printout t "Please choose between sunny, dry , regular  " crlf)
	(bind ?env (check-input daily_environment sunny dry regular))
	(send ?ins put-daily_environment ?env)
)		


(defrule read-skin_type 
?ins<-(object(is-a CUSTOMER)(cname ?nm))
=>
	(printout t "what's your skin type" crlf)
	(printout t "choose between 1 to 5" crlf)
	(printout t "1, oily" crlf)
	(printout t "2, dry" crlf)
	(printout t "3, combination" crlf)
	(printout t "4, acne prone" crlf)
	(printout t "5, sensitive" crlf)
	(bind ?ty (check-input 1 2 3 4 5))
	(send ?ins put-skin_type ?ty)
)

(defrule read-price_range (declare (salience -8))
?ins<-(object(is-a CUSTOMER)(cname ?nm))
?ins1<-(object(is-a PREFER))
=>
	(printout t "what's your price range" crlf)
	(printout t "choose between 1,2,3" crlf)
	(printout t "1, $1-50" crlf)
	(printout t "2, $50-150" crlf)
	(printout t "3, $150++" crlf)
	(bind ?ty (check-input 1 2 3))
	(send ?ins1 put-price_range ?ty)
)

(defrule age-young 
?ins<-(object(is-a CUSTOMER)(age ?ag))
?ins1<-(object(is-a PREFER))
(test (<= ?ag 25))
=>
	(send ?ins1 put-age_group young)
)

(defrule age-middle 
?ins<-(object(is-a CUSTOMER)(age ?ag))
?ins1<-(object(is-a PREFER))
(test (and (> ?ag 25)(<= ?ag 40)))
=>
	(send ?ins1 put-age_group middle)
)


(defrule age-old 
?ins<-(object(is-a CUSTOMER)(age ?ag))
?ins1<-(object(is-a PREFER))
(test (>= ?ag 40))
=>
	(send ?ins1 put-age_group old)
)





(defrule acne 
?ins<-(object(is-a CUSTOMER)(skin_type 4))
?ins2<-(object(is-a PREFER))
=>
	(printout t "conceren will be acne" crlf)
	(send ?ins2 put-concern acne)
)

(defrule sun 
?ins<-(object(is-a CUSTOMER)(daily_environment sunny))
?ins2<-(object(is-a PREFER))
=>
	(printout t "conceren will be sun_protection" crlf)
	(send ?ins2 put-concern sun_protection)
)

(defrule winkle
?ins2<-(object(is-a PREFER)(age_group old))
=> 
	(printout t "conceren will be winkle" crlf)
	(send ?ins2 put-concern winkle)
)

(defrule winkle1
?ins<-(object(is-a CUSTOMER)(daily_environment regular)(skin_type 2))
?ins2<-(object(is-a PREFER)(age_group middle))
=>
	(send ?ins2 put-concern winkle)
)

(defrule mois1
?ins<-(object(is-a CUSTOMER)(daily_environment dry)(skin_type 2))
?ins2<-(object(is-a PREFER)(age_group young))
=>
	(send ?ins2 put-concern mois)
)

(defrule mois2
?ins<-(object(is-a CUSTOMER)(daily_environment dry)(skin_type 2))
?ins2<-(object(is-a PREFER)(age_group middle))
=>
	(send ?ins2 put-concern mois)
)

(defrule mois3
?ins<-(object(is-a CUSTOMER)(daily_environment regular)(skin_type 2))
?ins2<-(object(is-a PREFER)(age_group young))
=>
	(send ?ins2 put-concern mois)
)

(defrule mois4
?ins<-(object(is-a CUSTOMER)(daily_environment dry)(skin_type 3))
?ins2<-(object(is-a PREFER)(age_group young))
=>
	(send ?ins2 put-concern mois)
)

(defrule winkle3
?ins<-(object(is-a CUSTOMER)(daily_environment dry)(skin_type 3))
?ins2<-(object(is-a PREFER)(age_group middle))
=>
	(send ?ins2 put-concern winkle)
)

(defrule oil_control1
?ins<-(object(is-a CUSTOMER)(daily_environment regular)(skin_type 3))
?ins2<-(object(is-a PREFER)(age_group young))
=>
	(send ?ins2 put-concern oil_control)
)

(defrule mois5
?ins<-(object(is-a CUSTOMER)(daily_environment regular)(skin_type 3))
?ins2<-(object(is-a PREFER)(age_group middle))
=>
	(send ?ins2 put-concern mois)
)

(defrule irritation
?ins<-(object(is-a CUSTOMER)(skin_type sensitive))
?ins2<-(object(is-a PREFER))
=>
	(send ?ins2 put-concern irritation_free)
)

(defrule oil_control
?ins<-(object(is-a CUSTOMER)(skin_type oily))
?ins2<-(object(is-a PREFER))
=>
	(send ?ins2 put-concern oil_control)
)

;;new rule
(defrule kiehl
?ins2<-(object(is-a PREFER)(concern winkle)(price_range 1)
=>
(printout t "Recommended cusmetic brand: Kiehl's anti aging series" crlf)
)

(defrule lancom
?ins2<-(object(is-a PREFER)(concern winkle)(price_range 2)
=>
(printout t "Recommended cusmetic brand: Lancom" crlf)
)

(defrule lap
?ins2<-(object(is-a PREFER)(concern winkle)(price_range 2)
=>
(printout t "Recommended cusmetic brand: La Prairie " crlf)
)

(defrule acnefree
?ins2<-(object(is-a PREFER)(concern acne)(price_range 1)
=>
(printout t "Recommended cusmetic brand: acnefree " crlf)
)

(defrule lar
?ins2<-(object(is-a PREFER)(concern acne)(price_range 2)
=>
(printout t "Recommended cusmetic brand: la roche " crlf)
)

(defrule lamer
?ins2<-(object(is-a PREFER)(concern acne)(price_range 3)
=>
(printout t "Recommended cusmetic brand: la mer " crlf)
)

(defrule firstaid
?ins2<-(object(is-a PREFER)(concern mois)(price_range 1)
=>
(printout t "Recommended cusmetic brand: First Aid Beauty " crlf)
)

(defrule phil
?ins2<-(object(is-a PREFER)(concern mois)(price_range 2)
=>
(printout t "Recommended cusmetic brand: Philosophy" crlf)
)

(defrule sisley
?ins2<-(object(is-a PREFER)(concern mois)(price_range 3)
=>
(printout t "Recommended cusmetic brand: Sisley " crlf)
)

(defrule clean
?ins2<-(object(is-a PREFER)(concern oil_control)(price_range 1)
=>
(printout t "Recommended cusmetic brand: clean&clear" crlf)
)

(defrule shesi
?ins2<-(object(is-a PREFER)(concern oil_control)(price_range 2)
=>
(printout t "Recommended cusmetic brand:shiseido" crlf)
)

(defrule quasar
?ins2<-(object(is-a PREFER)(concern oil_control)(price_range 3)
=>
(printout t "Recommended cusmetic brand: Baby Quasar Blue light" crlf)
)

(defrule ceraV
?ins2<-(object(is-a PREFER)(concern irritation)(price_range 1)
=>
(printout t "Recommended cusmetic brand: ceraVare" crlf)
)

(defrule ki_irri
?ins2<-(object(is-a PREFER)(concern irritation)(price_range 2)
=>
(printout t "Recommended cusmetic brand: Kiehl's tiger grass serial" crlf)
)

(defrule sis_irri
?ins2<-(object(is-a PREFER)(concern irritation)(price_range 3)
=>
(printout t "Recommended cusmetic brand: Sisley" crlf)
)



(defrule print-instance (declare (salience -10))
(object (is-a CUSTOMER)(cname ?nm) (daily_environment ?de)(skin_type ?st))
(object (is-a PREFER)(concern ?conc)(age_group ?agr)(price_range ?pr) )
=>
(printout t "customer name: " ?nm ", price_range :" ?pr ", age group:" ?agr ", skin type :"
 ?st  ", daily env: " ?de ", conceren: " ?conc  crlf)
)