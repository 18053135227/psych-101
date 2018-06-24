(TeX-add-style-hook
 "final_project_crit_learn"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("apa6" "jou" "apacite")))
   (TeX-run-style-hooks
    "latex2e"
    "apa6"
    "apa610"
    "amsmath"
    "color"
    "relsize"
    "float"
    "graphicx"
    "mathrsfs")
   (LaTeX-add-labels
    "space"
    "trial"
    "conditions")
   (LaTeX-add-bibliographies
    "sample"))
 :latex)

