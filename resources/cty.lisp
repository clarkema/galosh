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

(defparameter *entity-name->adif* (make-hash-table :test #'equal))
(defparameter *entity-adif->name* (make-hash-table))
(mapc #'(lambda (p)
	  (say p)
	  (setf (gethash (cdr p) *entity-name->adif*) (car p))
	  (setf (gethash (car p) *entity-adif->name*) (cdr p)))
      (list '(1 . "Canada")
	    '(3 . "Afghanistan")
	    '(4 . "Agalega & St Brandon")
	    '(5 . "Aland Is")
	    '(6 . "Alaska")
	    '(7 . "Albania")
	    '(9 . "American Samoa")
	    '(10 . "Amsterdam & St Paul")
	    '(11 . "Andaman & Nicobar Is")
	    '(12 . "Anguilla")
	    '(13 . "Antarctica")
	    '(14 . "Armenia")
	    '(15 . "Asiatic Russia")
	    '(16 . "Auckland & Campbell")
	    '(17 . "Aves Island")
	    '(18 . "Azerbaijan")
	    '(20 . "Baker, Howland Is")
	    '(21 . "Balearic Is")
	    '(22 . "Palau")
	    '(24 . "Bouvet")
	    '(27 . "Belarus")
	    '(29 . "Canary Is")
	    '(31 . "Central Kiribati")
	    '(32 . "Ceuta & Melilla")
	    '(33 . "Chagos")
	    '(34 . "Chatham Is")
	    '(35 . "Christmas Is")
	    '(36 . "Clipperton Is")
	    '(37 . "Cocos Island")
	    '(38 . "Cocos-Keeling Is")
	    '(40 . "Crete")
	    '(41 . "Crozet")
	    '(43 . "Desecheo Is")
	    '(45 . "Dodecanese")
	    '(46 . "East Malaysia")
	    '(47 . "Easter Is")
	    '(48 . "Eastern Kiribati")
	    '(49 . "Equatorial Guinea")
	    '(50 . "Mexico")
	    '(51 . "Eritrea")
	    '(52 . "Estonia")
	    '(53 . "Ethiopia")
	    '(54 . "European Russia")
	    '(56 . "Fernando De Noronha")
	    '(60 . "Bahamas")
	    '(61 . "Franz Josef Land")
	    '(62 . "Barbados")
	    '(63 . "French Guiana")
	    '(64 . "Bermuda")
	    '(65 . "British Virgin Is")
	    '(66 . "Belize")
	    '(69 . "Cayman Islands")
	    '(70 . "Cuba")
	    '(71 . "Galapagos")
	    '(72 . "Dominican Republic")
	    '(74 . "El Salvador")
	    '(75 . "Georgia")
	    '(76 . "Guatemala")
	    '(77 . "Grenada")
	    '(78 . "Haiti")
	    '(79 . "Guadeloupe")
	    '(80 . "Honduras")
	    '(82 . "Jamaica")
	    '(84 . "Martinique")
	    '(86 . "Nicaragua")
	    '(88 . "Panama")
	    '(89 . "Turks & Caicos Is")
	    '(90 . "Trinidad & Tobago")
	    '(91 . "Aruba")
	    '(94 . "Antigua & Barbuda")
	    '(95 . "Dominica")
	    '(96 . "Montserrat")
	    '(97 . "St Lucia")
	    '(98 . "St Vincent")
	    '(99 . "Glorioso Is")
	    '(100 . "Argentina")
	    '(103 . "Guam")
	    '(104 . "Bolivia")
	    '(105 . "Guantanamo Bay")
	    '(106 . "Guernsey")
	    '(107 . "Guinea")
	    '(108 . "Brazil")
	    '(109 . "Guinea-Bissau")
	    '(110 . "Hawaii")
	    '(111 . "Heard Is")
	    '(112 . "Chile")
	    '(114 . "Isle Of Man")
	    '(116 . "Colombia")
	    '(117 . "ITU HQ")
	    '(118 . "Jan Mayen")
	    '(120 . "Ecuador")
	    '(122 . "Jersey")
	    '(123 . "Johnston Is")
	    '(124 . "Juan De Nova")
	    '(125 . "Juan Fernandez")
	    '(126 . "Kaliningrad")
	    '(129 . "Guyana")
	    '(130 . "Kazakhstan")
	    '(131 . "Kerguelen")
	    '(132 . "Paraguay")
	    '(133 . "Kermadec")
	    '(134 . "Kingman Reef")
	    '(135 . "Kyrgyzstan")
	    '(136 . "Peru")
	    '(137 . "Republic Of Korea")
	    '(138 . "Kure Island")
	    '(140 . "Suriname")
	    '(141 . "Falkland Is")
	    '(142 . "Lakshadweep Islands")
	    '(143 . "Laos")
	    '(144 . "Uruguay")
	    '(145 . "Latvia")
	    '(146 . "Lithuania")
	    '(147 . "Lord Howe Is")
	    '(148 . "Venezuela")
	    '(149 . "Azores")
	    '(150 . "Australia")
	    '(151 . "Malyj Vysotski Is")
	    '(152 . "Macao")
	    '(153 . "Macquarie Is")
	    '(157 . "Nauru")
	    '(158 . "Vanuatu")
	    '(159 . "Maldives")
	    '(160 . "Tonga")
	    '(161 . "Malpelo Is")
	    '(162 . "New Caledonia")
	    '(163 . "Papua New Guinea")
	    '(165 . "Mauritius Is")
	    '(166 . "Mariana Is")
	    '(167 . "Market Reef")
	    '(168 . "Marshall Is")
	    '(169 . "Mayotte")
	    '(170 . "New Zealand")
	    '(171 . "Mellish Reef")
	    '(172 . "Pitcairn Is")
	    '(173 . "Micronesia")
	    '(174 . "Midway Is")
	    '(175 . "French Polynesia")
	    '(176 . "Fiji")
	    '(177 . "Minami Torishima")
	    '(179 . "Moldova")
	    '(180 . "Mount Athos")
	    '(181 . "Mozambique")
	    '(182 . "Navassa Is")
	    '(185 . "Solomon Islands")
	    '(187 . "Niger")
	    '(188 . "Niue")
	    '(189 . "Norfolk Is")
	    '(190 . "Samoa")
	    '(191 . "North Cook Is")
	    '(192 . "Ogasawara")
	    '(195 . "Annobon I.")
	    '(197 . "Palmyra & Jarvis Is")
	    '(199 . "Peter I Is")
	    '(201 . "Prince Edward & Marion")
	    '(202 . "Puerto Rico")
	    '(203 . "Andorra")
	    '(204 . "Revillagigedo")
	    '(205 . "Ascension Island")
	    '(206 . "Austria")
	    '(207 . "Rodriguez Is")
	    '(209 . "Belgium")
	    '(211 . "Sable Island")
	    '(212 . "Bulgaria")
	    '(213 . "Saint Martin")
	    '(214 . "Corsica")
	    '(215 . "Cyprus")
	    '(216 . "San Andres & Providencia")
	    '(217 . "San Felix")
	    '(219 . "Sao Tome & Principe")
	    '(221 . "Denmark")
	    '(222 . "Faroe Is")
	    '(223 . "England")
	    '(224 . "Finland")
	    '(225 . "Sardinia")
	    '(227 . "France")
	    '(230 . "Fed. Rep. Of Germany")
	    '(232 . "Somalia")
	    '(233 . "Gibraltar")
	    '(234 . "South Cook Is")
	    '(235 . "South Georgia Is")
	    '(236 . "Greece")
	    '(237 . "Greenland")
	    '(238 . "South Orkney Is")
	    '(239 . "Hungary")
	    '(240 . "South Sandwich Islands")
	    '(241 . "South Shetland Islands")
	    '(242 . "Iceland")
	    '(245 . "Ireland")
	    '(246 . "Sov. Military Order Of Malta")
	    '(247 . "Spratly Is")
	    '(248 . "Italy")
	    '(249 . "St. Kitts & Nevis")
	    '(250 . "St. Helena")
	    '(251 . "Liechtenstein")
	    '(252 . "St Paul Island")
	    '(253 . "St. Peter & St. Paul Rocks")
	    '(254 . "Luxembourg")
	    '(256 . "Madeira Is")
	    '(257 . "Malta")
	    '(259 . "Svalbard Is")
	    '(260 . "Monaco")
	    '(262 . "Tajikistan")
	    '(263 . "Netherlands")
	    '(265 . "Northern Ireland")
	    '(266 . "Norway")
	    '(269 . "Poland")
	    '(270 . "Tokelau Is")
	    '(272 . "Portugal")
	    '(273 . "Trindade & Martin Vaz Islands")
	    '(274 . "Tristan da Cunha & Gough Islands")
	    '(275 . "Romania")
	    '(276 . "Tromelin")
	    '(277 . "St. Pierre & Miquelon")
	    '(278 . "San Marino")
	    '(279 . "Scotland")
	    '(280 . "Turkmenistan")
	    '(281 . "Spain")
	    '(282 . "Tuvalu")
	    '(283 . "UK Bases on Cyprus")
	    '(284 . "Sweden")
	    '(285 . "US Virgin Islands")
	    '(286 . "Uganda")
	    '(287 . "Switzerland")
	    '(288 . "Ukraine")
	    '(289 . "United Nations HQ")
	    '(291 . "United States")
	    '(292 . "Uzbekistan")
	    '(293 . "Vietnam")
	    '(294 . "Wales")
	    '(295 . "Vatican")
	    '(296 . "Serbia")
	    '(297 . "Wake Is")
	    '(298 . "Wallis & Futuna")
	    '(299 . "West Malaysia")
	    '(301 . "Western Kiribati")
	    '(302 . "Western Sahara")
	    '(303 . "Willis Is")
	    '(304 . "Bahrain")
	    '(305 . "Bangladesh")
	    '(306 . "Bhutan")
	    '(308 . "Costa Rica")
	    '(309 . "Myanmar")
	    '(312 . "Cambodia")
	    '(315 . "Sri Lanka")
	    '(318 . "China")
	    '(321 . "Hong Kong")
	    '(324 . "India")
	    '(327 . "Indonesia")
	    '(330 . "Iran")
	    '(333 . "Iraq")
	    '(336 . "Israel")
	    '(339 . "Japan")
	    '(342 . "Jordan")
	    '(344 . "Democratic People's Republic Of Korea")
	    '(345 . "Brunei")
	    '(348 . "Kuwait")
	    '(354 . "Lebanon")
	    '(363 . "Mongolia")
	    '(369 . "Nepal")
	    '(370 . "Oman")
	    '(372 . "Pakistan")
	    '(375 . "Philippines")
	    '(376 . "Qatar")
	    '(378 . "Saudi arabia")
	    '(379 . "Seychelles")
	    '(381 . "Singapore")
	    '(382 . "Djibouti")
	    '(384 . "Syria")
	    '(386 . "Taiwan")
	    '(387 . "Thailand")
	    '(390 . "Turkey")
	    '(391 . "United Arab Emirates")
	    '(400 . "Algeria")
	    '(401 . "Angola")
	    '(402 . "Botswana")
	    '(404 . "Burundi")
	    '(406 . "Cameroon")
	    '(408 . "Central African Republic")
	    '(409 . "Cape Verde")
	    '(410 . "Chad")
	    '(411 . "Comoros")
	    '(412 . "Republic Of The Congo")
	    '(414 . "Dem. Republic Of The Congo")
	    '(416 . "Benin")
	    '(420 . "Gabon")
	    '(422 . "The Gambia")
	    '(424 . "Ghana")
	    '(428 . "Cote d'Ivoire")
	    '(430 . "Kenya")
	    '(432 . "Lesotho")
	    '(434 . "Liberia")
	    '(436 . "Libya")
	    '(438 . "Madagascar")
	    '(440 . "Malawi")
	    '(442 . "Mali")
	    '(444 . "Mauritania")
	    '(446 . "Morocco")
	    '(450 . "Nigeria")
	    '(452 . "Zimbabwe")
	    '(453 . "Reunion")
	    '(454 . "Rwanda")
	    '(456 . "Senegal")
	    '(458 . "Sierra Leone")
	    '(460 . "Rotuma Is")
	    '(462 . "Republic Of South Africa")
	    '(464 . "Namibia")
	    '(466 . "Sudan")
	    '(468 . "Swaziland")
	    '(470 . "Tanzania")
	    '(474 . "Tunisia")
	    '(478 . "Egypt")
	    '(480 . "Burkina Faso")
	    '(482 . "Zambia")
	    '(483 . "Togo")
	    '(489 . "Conway Reef")
	    '(490 . "Banaba Island")
	    '(492 . "Yemen")
	    '(497 . "Croatia")
	    '(499 . "Slovenia")
	    '(501 . "Bosnia-Herzegovina")
	    '(502 . "Macedonia")
	    '(503 . "Czech Republic")
	    '(504 . "Slovak Republic")
	    '(505 . "Pratas Is")
	    '(506 . "Scarborough Reef")
	    '(507 . "Temotu Province")
	    '(508 . "Austral Is")
	    '(509 . "Marquesas Is")
	    '(510 . "Palestine")
	    '(511 . "Timor-Leste")
	    '(512 . "Chesterfield Is")
	    '(513 . "Ducie Is")
	    '(514 . "Montenegro")
	    '(515 . "Swains Island")
	    '(516 . "St. Barthelemy")
	    '(517 . "Curacao")
	    '(518 . "Sint Maarten")
	    '(519 . "St. Eustatius And Saba")
	    '(520 . "Bonaire")
	    '(521 . "The Republic Of South Sudan")))
