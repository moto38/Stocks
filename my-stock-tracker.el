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

(message (my-stock-tracker--request-synchronously my-stock-tracker--api-url "4661"))


(switch-to-buffer
           (url-retrieve-synchronously
            (format stock-url stock) t nil 5))

	   


(defun stock-tracker--request-synchronously (stock tag)
  "Get STOCK data with TAG synchronously, return a list of JSON each as alist."
  (let* (jsons response)
    (ignore-errors
      (with-current-buffer
          (url-retrieve-synchronously
           (format (stock-tracker--api-url tag) (url-hexify-string stock)) t nil 5)
        (set-buffer-multibyte t)
        (goto-char (point-min))
        (when (string-match "200 OK" (buffer-string))
          (re-search-forward (stock-tracker--result-prefix tag) nil 'move)
          (setq response (buffer-substring-no-properties (point) (point-max)))
	  (setq jsons (json-read-from-string (replace-regexp-in-string "[\\[\\]]" "" response)))
	  )
        (kill-current-buffer)))
    jsons))

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

