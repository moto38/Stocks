(defvar my-stock-tracker--api-url "https://query2.finance.yahoo.com/v8/finance/chart/%s.T"
  "API to get stock for S from YahooFinanceAPI." )


(setq my-stock-tracker--list-of-stocks
      (list
       "4661"   ;; OLTC
       "8411"   ;; MFG
       )
      )

(setq stock-url my-stock-tracker--api-url)
(setq stock "4661")

(defun epoch2date (unix-time)
  "Epoch time to human readble format."
  (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time unix-time))
  )


(defun my-stock-tracker--request-synchronously (stock-url stock)
  "Get STOCK data with stock-url + stock synchronously, return a list of JSON each as alist."
  (let* (jsons response)
    (ignore-errors
      (with-current-buffer
          (url-retrieve-synchronously
           (format stock-url stock) t nil 5)
	(set-buffer-multibyte t)
        (goto-char (point-min))
        (when (string-match "200 OK" (buffer-string))
	  ;;(message "=== 200 OK===\n")
          (when (re-search-forward "^{" nil 'move)
	    (goto-char (match-beginning 0)))
          (setq response (buffer-substring-no-properties (point) (point-max)))
	  )
	;;(kill-current-buffer)
	))
    ;;jsons))
    response))


(type-of
(hash-table-keys

 (gethash "regularMarketPrice"
 (gethash "meta"
	  (aref
	   (gethash "result"
		    (gethash "chart"
			     (json-parse-string (my-stock-tracker--request-synchronously my-stock-tracker--api-url "4661"))
			     )
		    )
	   0)
	  )
 )

 )
)


;; ("indicators" "timestamp" "meta")
(setq json-main-data
      (aref
       (gethash "result"
		(gethash "chart"
			 (json-parse-string (my-stock-tracker--request-synchronously my-stock-tracker--api-url "4661"))
			 )
		)
       0)
      )

(length
 (gethash "open"
  (aref (gethash "quote" (gethash "indicators" json-main-data)) 0)
  )

;; indicators => quote => ("open" "high" "volume" "close" "low")

)

(setq my-stock-tracker-meta (gethash "meta" json-main-data))
(setq my-stock-tracker-timestamps (gethash "timestamp" json-main-data))
(setq my-stock-tracker-quote   (aref (gethash "quote" (gethash "indicators" json-main-data)) 0))


(defun my-stock-tracker-check ()
  (let ((check (-
	       (aref (gethash "close" my-stock-tracker-quote) (1- (length my-stock-tracker-timestamps)))
	       (map-elt my-stock-tracker-meta "regularMarketPrice"))))
    (cond ((> check 0) "up")
	  ((< check 0) "down")
	  (t "stay"))))

(my-stock-tracker-check)


(setq my-stock-tracker-list-dates
      (lambda (h)
	(dotimes (i (length h))
	  (let ((element (aref h i)))
	    (message "%d => %s" element (epoch2date element) )
	    ))))

(funcall my-stock-tracker-list-dates (gethash "timestamp" json-main-data))

json-main-data
(map-elt json-main-data "meta")
(epoch2date 1729209600)  ;;"2024-10-18 09:00:00"
(epoch2date 1729231200)  ;;"2024-10-18 15:00:00"


(map-elt my-stock-tracker-meta "symbol")
(map-elt my-stock-tracker-meta "shortName")
(map-elt my-stock-tracker-meta "exchangeName")
(map-elt my-stock-tracker-meta "previousClose")
(map-elt my-stock-tracker-meta "regularMarketPrice")
(map-elt my-stock-tracker-meta "regularMarketDayHigh")
(map-elt my-stock-tracker-meta "regularMarketDayLow")
(map-elt my-stock-tracker-meta "regularMarketVolume")
(epoch2date (map-elt my-stock-tracker-meta "regularMarketTime"))
(epoch2date (aref my-stock-tracker-timestamps (1- (length my-stock-tracker-timestamps))))
(aref (gethash "close" my-stock-tracker-quote) (1- (length my-stock-tracker-timestamps)))
(my-stock-tracker-check)



;; meta hash-table-key
;;("validRanges"
;; "range"
;; "dataGranularity"
;; "tradingPeriods"
;; "currentTradingPeriod"
;; "priceHint"
;; "scale"
;; "previousClose"        前日の終値(まあ、こっち)
;; "chartPreviousClose"
;; "shortName"
;; "longName"
;; "regularMarketVolume"
;; "regularMarketDayLow"  通常時間取引中の最安値
;; "regularMarketDayHigh" 通常時間取引中の最高値
;; "fiftyTwoWeekLow"
;; "fiftyTwoWeekHigh"
;; "regularMarketPrice"   通常時間中の最新の株価
;; "exchangeTimezoneName"
;; "timezone"
;; "gmtoffset"
;; "hasPrePostMarketData"
;; "regularMarketTime"    通常取引時間内の最後に取引が行われた時刻
;; "firstTradeDate"
;; "instrumentType"
;; "fullExchangeName"
;; "exchangeName"
;; "symbol"
;; "currency")


 
;; meta
;;#s(hash-table size 28 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;; ("currency" "JPY"
;;  "symbol" "4661.T"
;;  "exchangeName" "JPX"
;;  "fullExchangeName" "Tokyo"
;;  "instrumentType" "EQUITY"
;;  "firstTradeDate" 978307200
;;  "regularMarketTime" 1729232100
;;  "hasPrePostMarketData" :false
;;  "gmtoffset" 32400
;;  "timezone" "JST"
;;  "exchangeTimezoneName" "Asia/Tokyo"
;;  "regularMarketPrice" 3547.0
;;  "fiftyTwoWeekHigh" 3566.0
;;  "fiftyTwoWeekLow" 3526.0
;;  "regularMarketDayHigh" 3566.0
;;  "regularMarketDayLow" 3526.0
;;  "regularMarketVolume" 2240600
;;  "longName" "Oriental Land Co., Ltd."
;;  "shortName" "ORIENTAL LAND CO"
;;  "chartPreviousClose" 3530.0
;;  "previousClose" 3530.0
;;  "scale" 3
;;  "priceHint" 2
;;  "currentTradingPeriod" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;    ("pre" #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;      ("timezone" "JST" "start" 1729209600 "end" 1729209600 "gmtoffset" 32400))
;;     "regular" #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;      ("timezone" "JST" "start" 1729209600 "end" 1729231200 "gmtoffset" 32400))
;;     "post" #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;      ("timezone" "JST" "start" 1729231200 "end" 1729231200 "gmtoffset" 32400))))
;;  "tradingPeriods" [[#s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;    ("timezone" "JST" "start" 1729209600 "end" 1729231200 "gmtoffset" 32400))]]
;;  "dataGranularity" "1m"
;;  "range" "1d"
;;  "validRanges" ["1d" "5d" "1mo" "3mo" "6mo" "1y" "2y" "5y" "10y" "ytd" "max"]
;; )
;;)

(defconst stock-tracker--result-header
  "|-\n| symbol | name | price | percent | updown | high | low | volume | open | yestclose |\n"
  "Stock-Tracker result header.")

(defconst stock-tracker--result-item-format
  "|-\n| %s | %s | %s | %.2f %% | %.2f | %s | %s | %s | %s | %.2f |\n"
  "Stock-Tracker result item format.")

(defconst stock-tracker--response-buffer "*api-response*"
  "Buffer name for error report when fail to read server response.")


(defconst stock-tracker--header-string
  "* Stocks refreshed at: [ %current-time% ] auto-refreshing is: [ %refresh-state% ]"
  "Stock-Tracker header string.")

(defconst stock-tracker--note-string
  (purecopy
   "** Add     stock, use [ *a* ]
** Delete  stock, use [ *d* ]
** Start refresh, use [ *g* ]
** Stop  refresh, use [ *s* ]
** Stocks listed in SH, prefix with [ *0* ], e.g: 0600000
** Stocks listed in SZ, prefix with [ *1* ], e.g: 1002024
** Stocks listed in US,                    e.g: GOOG")
  "Stock-Tracker note string.")






(defun stock-tracker--format-json (json tag)
  "Format stock information from JSON with TAG."
  (let ((result-filds (stock-tracker--result-fields tag))
        symbol name price percent (updown 0) color
        high low volume open yestclose code)

    (setq
      code      (assoc-default (map-elt result-filds 'code)      json)
      symbol    (assoc-default (map-elt result-filds 'symbol)    json)
      name      (assoc-default (map-elt result-filds 'name)      json) ; chinese-word failed to align
      price     (assoc-default (map-elt result-filds 'price)     json)
      percent   (assoc-default (map-elt result-filds 'percent)   json)
      updown    (assoc-default (map-elt result-filds 'updown)    json)
      open      (assoc-default (map-elt result-filds 'open)      json)
      yestclose (assoc-default (map-elt result-filds 'yestclose) json)
      high      (assoc-default (map-elt result-filds 'high)      json)
      low       (assoc-default (map-elt result-filds 'low)       json)
      volume    (assoc-default (map-elt result-filds 'volume)    json))

    ;; sanity check
    (unless (and symbol name price percent updown open yestclose high low volume)
      (stock-tracker--log "Invalid data received !!!")
      (throw 'break 0))

    ;; formating
    (and (stringp percent)   (setq percent (string-to-number percent)))
    (and (stringp volume)    (setq volume (string-to-number volume)))
    (and (stringp yestclose) (setq yestclose (string-to-number yestclose)))
    (and (stringp updown)    (setq updown (string-to-number updown)))

    ;; color setting
    (if stock-tracker-up-red-down-green
        (if (> updown 0) (setq color "red") (setq color "green"))
      (if (> updown 0) (setq color "green") (setq color "red")))

    ;; some extra handling
    (and (cl-typep tag 'stock-tracker--chn-symbol) (setq percent (* 100 percent)))

    ;; construct data for display
    (and symbol
         (propertize
          (format stock-tracker--result-item-format symbol
                  name price percent updown high low
                  (stock-tracker--add-number-grouping volume ",")
                  open yestclose)
          'stock-code  code
          'stock-color color))))

(defun stock-tracker--format-response (response tag &optional asynchronously)
  "Format stock information from RESPONSE with TAG, with optional ASYNCHRONOUSLY."
  (let ((jsons response)
        (result "") result-list)
    (catch 'break
      ;; handle difference in async handling
      (and asynchronously
           (cl-typep tag 'stock-tracker--chn-symbol)
           (setq jsons (car jsons)))

      (dolist (json jsons)
        (if (cl-typep tag 'stock-tracker--chn-symbol)
            (setq json (cdr json))
          ;; for us-stock, there's only one stock data here
          (setq json jsons))

        (when-let ((info (stock-tracker--format-json json tag)))
          (push info result-list))

        ;; for us-stock, there's only one stock data here
        (unless (cl-typep tag 'stock-tracker--chn-symbol)
          (throw 'break t))))
    (and result-list
         (setq result (stock-tracker--list-to-string (reverse result-list) "")))
    result))

(format "aa%s.T" "4461")

