let rec range lower upper =
  if lower < upper then lower :: range (lower + 1) upper else [ upper ]
;;
