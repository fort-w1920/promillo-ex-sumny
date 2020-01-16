#' Calculate the blood alcohol level
#'
#' Calculate the blood alcohol level depending on your age, sex, height,
#' weight, drinking_time, and the drinks you had.

#' @param age age in years, numeric
#' @param sex sex ("male" vs. "female"), character
#' @param height height in cm, numeric
#' @param weight weight in kg, numeric
#' @param drinking_time vector of start and end time, POSIXct
#' @param drinks list or vector of drinks, named numeric, supported drinks are
#'   "massn", "hoibe", "wein", "schnaps"
#' @return blood alcohol level, numeric
#' @importFrom checkmate assert_subset assert_numeric assert_number assert_posixct
#' @md
#' @export
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}

# utilities --------------------------------------------------------------------

#' Calculate the alcohol level of some drinks
#'
#' Based on the drinks supplied, calculate the overall alcohol level.
#'
#' @inheritParams tell_me_how_drunk
#' @return blood alcohol level, numeric
#' @md
get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  assert_subset(names(drinks), choices = c("massn", "hoibe", "wein", "schnaps"),
    empty.ok = FALSE)
  assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40)
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4)
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
      alcohol_concentration[names(drinks)] * alcohol_density)
}

#' Calculate the bodywater level
#'
#' Based on sex, age, height and weight, calculate the bodywater level.
#'
#' @inheritParams tell_me_how_drunk
#' @return bodywater level, numeric
#' @md
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)

  assert_number(age, lower = 10, upper = 110)
  if (age < 16 | age > 90) {
    warning("...ts ts ts, this at your age! (illegal)")
  }
  assert_number(height, lower = 100, upper = 230)
  assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}

#' Calculate the permille
#'
#' Using the alcohol level and bodywater level, calculate the permille.
#'
#' @param alcohol_drunk alcohol level, see [get_alcohol]
#' @param bodywater bodywater level, see [get_bodywater]
#' @inheritParams tell_me_how_drunk
#' @md
get_permille <- function(alcohol_drunk, bodywater, drinking_time) {
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille  - (max(0, partylength - 1) * sober_per_hour))
}

#' Plot the permille over the drinking time
#'
#' Plot the permille over the drinking time with the x-axis resembling
#' intervals of five minutes.
#'
#' @inheritParams tell_me_how_drunk
#' @return qplot plotting the permille over the drinking time (x-axis in five
#'   minutes intervals)
#' @importFrom ggplot2 qplot
#' @md
#' @export
show_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks) {
  # check drinking_time
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  # get the time difference
  partylength <- difftime(drinking_time[2L], drinking_time[1L], units = "mins")

  # make a copy of drinking_time
  drinking_interval <- drinking_time

  # and set the end point to the start point
  drinking_interval[2L] <- drinking_interval[1L]

  # generate 5 minute intervals based on the time difference
  intervals <- seq(from = 5L, to = as.numeric(partylength), by = 5L) * 60L

  # then calculate the permille by sequentially updating the drinking_interval
  # endpoint by adding 5 minutes
  permille <- vapply(intervals,
    FUN = function(x) {
      drinking_interval[2L] <- drinking_interval[2L] + x
      tell_me_how_drunk(age = age, sex = sex, height = height, weight = weight,
                        drinking_time = drinking_interval, drinks = drinks)
    }, FUN.VALUE = numeric(1L))

  # plot this
  qplot(y = permille, xlab = "Drinking Time (Intervals of 5 Minutes)",
    ylab = "Permille")
}

