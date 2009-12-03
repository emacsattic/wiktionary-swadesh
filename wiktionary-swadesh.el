;;;; wiktionary-swadesh.el -- Parse a Swadesh list page from wiktionary, and convert it to Mulvo format
;;; Time-stamp: <2006-01-25 10:41:20 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'wiktionary-swadesh)
(require 'read-table-data)
(require 'csv-writer)

(add-to-list 'auto-coding-regexp-alist
	     (cons "content=\"text/html; charset=utf-8\""
		   'utf-8-unix))

(defun swadesh-actual-data (cell)
  "Return the actual data from CELL."
  (mapconcat (lambda (raw)
	       (subst-char-in-string
		?\n ?  (let ((href (string-match "<a[^>]*>\\([^<]*\\)</a>" raw)))
			 (if href
			     (substring raw (match-beginning 1) (match-end 1))
			   raw)) t))
	     (split-string (third cell) ",")
	     ","))

(defvar swadesh-name-alist
  '(("No" . "Swadesh_number")
    ("English" . "ENG")
    ("French" . "FRN")
    ("German" . "GER")
    ("Italian" . "ITN")
    ("Spanish" . "SPN")
    ("Dutch" . "DUT")
    ("Esperanto" . "ESP")
    ("Swedish" . "SWD")
    ("Occitan" . "PRV")
    ("Norwegian" . "NRR")
    ("Polish" . "PQL")
    ("Irish" . "GLI")
    ("Russian" . "RUS")
    ("Japanese" . "JPN")
    ("Finnish" . "FIN")
    ("Basque" . "BSQ")
    ("Breton" . "BRT")
    ("Welsh" . "WLS")
    ("Scottish Gaelic" . "GLS")
    ("Manx" . "MJD")
    ("Cornish" . "CRN")
    ("Hungarian" . "HNG")
    ("Finnish" . "FIN")
    ("Estonian" . "EST")
    ("Low Saxon" . "SXN")
    ("Icelandic" . "ICE")
    ("Latin" . "LTN"))
  "How the wiktionary names map onto the ones we use.")

(defun swadesh-parse-file (file)
  "Read a Swadesh list from FILE.
Returns data in the same format as csv-parse-buffer."
  (let* ((table (read-table-from-file file))
	 (headings (apply 'vector
			  (mapcar (lambda (sw-name)
				    (let ((pair (assoc sw-name swadesh-name-alist)))
				      (if pair (cdr pair) sw-name)))
				  (mapcar 'swadesh-actual-data (car table)))))
	 (maxcol (1- (length headings)))
	 (data (mapcar (lambda (row)
			 (apply 'vector (mapcar 'swadesh-actual-data row)))
		       (cdr table)))
	 (result nil))
    (message "Got languages %S from %s" headings file)
    (dolist (row data)
      (let ((i maxcol)
	    (row-pairs nil))
	(while (>= i 0)
	  (push (cons (aref headings i)
		      (aref row i))
		row-pairs)
	  (decf i))
	(push row-pairs result)))
    (setq result (nreverse result))
    result))

(defun swadesh-convert-wiktionary-to-csv (wiki-file csv-file)
  "Read a Swadesh list from FILE, and produce a corresponding CSV file."
  (interactive
   (let* ((wiki-file-name (read-file-name "Convert Swadesh file: " nil nil t))
	  (wiki-file-directory (file-name-directory wiki-file-name))
	  (base-file-name (file-name-nondirectory (file-name-sans-extension wiki-file-name)))
	  (csv-file-default-name (concat base-file-name ".csv"))
	  (csv-file-name (read-file-name "Write to CSV file: "
					 (car mulvo-dictionaries-directories)
					 csv-file-default-name
					 nil csv-file-default-name)))
     (list wiki-file-name csv-file-name)))
  (let ((data (swadesh-parse-file wiki-file)))
    (csv-write-data-to-file csv-file data 'utf-8-unix)))

;;; end of wiktionary-swadesh.el
