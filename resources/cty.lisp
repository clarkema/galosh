(predicate "KG4" #'(lambda (c) (if (= (length c) 5) t nil)))
(filter "SV" #'(lambda (f e)
		 (cond
		   ((ends-with-subseq "/5" f)
		    (make-instance 'entity
				   :prefix "SV/5"
				   :name "DODECANESE"
				   :adif "45"
				   :cq-zone "20"
				   :itu-zone "28"
				   :latitude "36.40"
				   :longitude "28.20"
				   :start nil
				   :end nil))
		   ((ends-with-subseq "/9" f)
		    (make-instance 'entity
				   :prefix "SV/9"
				   :name "CRETE"
				   :adif "40"
				   :cq-zone "20"
				   :itu-zone "28"
				   :latitude "35.40"
				   :longitude "25.20"
				   :start nil
				   :end nil))
		   (t e))))

(filter '("W" "K" "AH6" "KH6" "AH7" "KH7" "AL" "N")
	#'(lambda (f e)
	    (cond
	      ((cl-ppcre:scan "/W?\\d$" f)
	       (make-instance 'entity
			      :prefix "W"
			      :name "United States"
			      :adif "291"
			      :cq-zone "3"
			      :latitude "43.00"
			      :longitude "-87.90"))
	      (t e))))