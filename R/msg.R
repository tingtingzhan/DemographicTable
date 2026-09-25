

# @examples
# msg_logical()

msg_logical <- \() {
  
  sprintf(
    fmt = 'Some scientists do not understand %s value\f\u21ac %s = c(%s, %s).',
    sprintf(fmt = '{.fun %s::%s}', 'base', 'logical'),
    'arm_intervention' |> col_cyan() |> style_bold() |> style_italic(),
    'TRUE' |> col_red(), 
    'FALSE' |> col_blue()
  ) |>
    cli_text() |> # will ignore '\n' (but respect '\f')
    message(appendLF = FALSE) # seems needed after ?cli::cli_text
  
  sprintf(
    fmt = 'Consider using 2-level %s\f\u21ac %s = c(\'%s\', \'%s\').',
    sprintf(fmt = '{.fun %s::%s}', 'base', 'factor'),
    'arm' |> col_magenta() |> style_bold() |> style_italic(),
    'intervention' |> col_yellow(), 'control' |> col_green()
  ) |>
    cli_text() |> # will ignore '\n'
    message(appendLF = FALSE) # seems needed after ?cli::cli_text
  
}

