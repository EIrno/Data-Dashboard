package cryptoDashboard.services.utils

import java.text.DecimalFormat


object Formatter:

  def formatDollarNumber(num: Double): String =
    val f = new DecimalFormat("0.00")
    if num == 0 then 0.toString + " $"
    else if Math.abs(num) < 1 then new DecimalFormat("0.00E0").format(num) + " $"
    else if ((num / 1000000000.0) >= 1) then f.format(num / 1000000000.0) + " B $"
    else if ((num / 1000000.0) >= 1) then f.format(num / 1000000.0) + " M $"
    else f.format(num) + " $"