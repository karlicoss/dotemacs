; assumes with-eval-after-load 'python

(with-eval-after-load 'importmagic
  (setq importmagic-style-configuration-alist
    '((multiline   . backslash)
      (max_columns . 150))))

