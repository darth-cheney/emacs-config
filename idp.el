(defun idp/trans-test ()
  (interactive)
  (message "TEST"))

(define-transient-command idp/main-menu
  "Main Menu for IDP Mode"
  [:description "poop"
                ["Poop Poop, Lemonade" ["Subgroup" ("t" "test" idp/trans-test)]]])
