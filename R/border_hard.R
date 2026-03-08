


# hard border
.border_hard <- \() {
  fp_border(
    width = 2 * init_flextable_defaults()$border.width, # *looks* like default border width used in ?flextable::flextable
    color = init_flextable_defaults()$border.color # '#666666', i.e., 'gray40'
  )
}

