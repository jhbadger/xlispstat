#+macintosh(require ":data:tutorial")
#-macintosh(load-data "tutorial")

(scatterplot-matrix 
 (list hardness tensile-strength abrasion-loss)
 :variable-labels '("Hardness" "Tensile Strength" "Abrasion Loss"))

(spin-plot (list hardness tensile-strength abrasion-loss)
           :variable-labels '("H" "T" "A"))
