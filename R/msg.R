

#' @title  \link[base]{warning} Messages on the use of \link[base]{logical} variable
#' 
#' @examples
#' msg_logical()
#' 
#' @keywords internal
#' @export
msg_logical <- function() {
  
  sprintf(
    fmt = 'Some scientists do not understand %s value, e.g., %s being %s/%s.',
    sprintf(fmt = '{.fun %s::%s}', 'base', 'logical'),
    'arm_intervention' |> col_cyan() |> style_bold() |> style_italic(),
    'TRUE' |> col_red(), 'FALSE' |> col_blue()
  ) |>
    cli_text() |> # will ignore '\n'
    message(appendLF = FALSE) # seems needed after ?cli::cli_text
  
  sprintf(
    fmt = 'Consider using 2-level %s, e.g., %s being %s/%s.',
    sprintf(fmt = '{.fun %s::%s}', 'base', 'factor'),
    'arm' |> col_magenta() |> style_bold() |> style_italic(),
    'intervention' |> col_yellow(), 'control' |> col_green()
  ) |>
    cli_text() |> # will ignore '\n'
    message(appendLF = FALSE) # seems needed after ?cli::cli_text
  
}

