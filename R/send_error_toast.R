#' send_toast
#'
#' Send toast.
#' @param text Text of the toast.
#' @param title Title of the toast.
#' @keywords toast
#' @examples
#' \dontrun{
#'send_toast("You got toast")
#'}
#' @export

# Send toast
send_toast <- function(text, title) {

  # Test if Powershell is not found
  stopifnot(Sys.which("powershell.exe") != "")

  # Create powershell script
  ps_script <- paste(
    "Add-Type -AssemblyName System.Windows.Forms",
    "$global:balloon = New-Object System.Windows.Forms.NotifyIcon",
    "$path = (Get-Process -id $pid).Path",
    "$balloon.Icon = [System.Drawing.Icon]::ExtractAssociatedIcon($path)",
    "$balloon.BalloonTipIcon = [System.Windows.Forms.ToolTipIcon]::Warning",
    paste0("$balloon.BalloonTipText = '", text, "'"),
    paste0("$balloon.BalloonTipTitle = '", title, "'"),
    "$balloon.Visible = $true",
    "$balloon.ShowBalloonTip(10000)", sep = ";")

  # Activate poweshell script
  system(paste('powershell.exe -command', ps_script))
}

#' send_error_toast
#'
#' Send error toast.
#' @param text Text of the toast. Default: This is a toast
#' @param title Title of the toast. Default: Alert!
#' @keywords toast, error
#' @examples
#' \dontrun{
#'send_error_toast("Something went wrong")
#'stop("Error")
#'}
#' @export

# Send toast on error
send_error_toast <- function(
  text = "This is a toast with error:", title = "Alert!") {

  # Return option with send_error_toast function
  return(options(error = function() {
    send_toast(paste(text, geterrmessage()), title)
    }))
}
