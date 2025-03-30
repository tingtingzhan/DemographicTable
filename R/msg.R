

#' @title Various \link[base]{warning} and/or \link[base]{stop} Messages
#' 
#' @examples
#' cat(msg_logical())
#' @keywords internal
#' @name msg_
#' @importFrom cli style_bold style_underline col_cyan col_yellow col_magenta
#' @export
msg_logical <- function() {
  sprintf(fmt = '\nSome scientists do not understand %s value (e.g., %s being %s/%s)\nConsider using 2-level %s (e.g., %s being %s/%s)',
          'logical' |> style_underline() |> style_bold(),
          'arm_intervention' |> col_cyan() |> style_bold(),
          'TRUE' |> col_yellow(), 'FALSE' |> col_yellow(),
          'factor' |> style_underline() |> style_bold(),
          'arm' |> col_magenta() |> style_bold(),
          'intervention' |> col_yellow(), 'control' |> col_yellow())
}

# add a `\n` upfront to make sure the error message starts on a new line

