######## Course info ########
library(tidyverse)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Start of semester
start_semester <- "2026-03-02"

# Week of mid-semester break
mid_semester_break <- "2026-04-06"

# Schedule
schedule <- tibble(
  Week = seq(12),
  Topic = c(
    "Introduction to forecasting and R",
    "Time series graphics",
    "Time series decomposition",
    "The forecaster's toolbox",
    "Exponential smoothing",
    "Exponential smoothing",
    "ARIMA models",
    "ARIMA models",
    "ARIMA models",
    "Multiple regression and forecasting",
    "Dynamic regression",
    "Revision"
  ),
  Chapter = c(
    "1. Getting started",
    "2. Time series graphics",
    "3. Time series decomposition",
    "5. The forecaster's toolbox",
    "8. Exponential smoothing",
    "8. Exponential smoothing",
    "9. ARIMA models",
    "9. ARIMA models",
    "9. ARIMA models",
    "7. Time series regression models",
    "10. Dynamic regression models",
    "Revision"
  ),
  Chapter_URL = c(
    "https://OTexts.com/fpp3/intro.html",
    "https://OTexts.com/fpp3/graphics.html",
    "https://OTexts.com/fpp3/decomposition.html",
    "https://OTexts.com/fpp3/toolbox.html",
    "https://OTexts.com/fpp3/expsmooth.html",
    "https://OTexts.com/fpp3/expsmooth.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/regression.html",
    "https://OTexts.com/fpp3/dynamic.html",
    ""
  )
)

# Add mid-semester break
calendar <- tibble(
    Date = seq(as.Date(start_semester), by = "1 week", length.out = 13)
  ) |>
  mutate(
    Week = row_number(),
    Week = if_else(Date < mid_semester_break, Week, Week - 1),
    #Week =
  )

# Add calendar to schedule
schedule <- schedule |>
  left_join(calendar, by = "Week") |>
  mutate(
    Week = if_else(Date == mid_semester_break, NA, Week),
    Topic = if_else(Date == mid_semester_break, "Mid-semester break", Topic),
    Chapter = if_else(Date == mid_semester_break, NA, Chapter),
    Chapter_URL = if_else(Date == mid_semester_break, NA, Chapter_URL)
  ) |>
  select(Week, Date, everything())

# Add assignment details
lastmon <- function(x) {
  7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")
}

assignments <- read_csv(here::here("assignments.csv")) |>
  mutate(
    Date = lastmon(Due),
    Moodle = paste0(
      "https://learning.monash.edu/mod/",
      c("quiz", rep("assign", 3)), "/view.php?id=", Moodle
    ),
    Moodle3231 = paste0(
      "https://learning.monash.edu/mod/",
      c("quiz", rep("assign", 3)), "/view.php?id=", Moodle3231
    ),
    File = paste0("assignments/", File)
  )

quizzes <- read_csv(here::here("quizzes.csv")) |>
  mutate(
    Date = lastmon(QDue),

    QMoodle3231 = paste0(
      "https://learning.monash.edu/mod/quiz/view.php?id=",
      QMoodle3231
    ),

    QMoodle5231 = paste0(
      "https://learning.monash.edu/mod/quiz/view.php?id=",
      QMoodle5231
    )
  )

schedule <- schedule |>
  left_join(assignments, by = "Date") %>%
  left_join(quizzes, by = "Date")

week_metadata <- tibble(
  Week = 1:12,
  Summary = c(
    "Start with the forecasting mindset, the course toolkit, and the basic tsibble workflow used throughout the unit.",
    "Use graphical tools to surface trend, seasonality, noise, and structural features in time series data.",
    "Break time series into interpretable components so trend, seasonality, and remainder can be analysed separately.",
    "Build the practical forecasting toolkit: benchmark methods, transformations, regression with seasonality, and evaluation.",
    "Introduce exponential smoothing models for level and trend, and connect the methods to ETS notation.",
    "Extend ETS to handle seasonality and automatic model selection using the AICc.",
    "Begin ARIMA with stationarity, differencing, and random walk style models.",
    "Fit and compare AR, MA, ARMA, and ARIMA models using both manual and automatic approaches.",
    "Move from basic ARIMA to seasonal ARIMA and compare ARIMA forecasts against ETS alternatives.",
    "Use regression with external predictors to explain and forecast time series behaviour.",
    "Combine regression structure with ARIMA errors and dynamic harmonic regression for richer forecasting models.",
    "Use the final week to consolidate core methods, revisit difficult topics, and prepare for the exam and final submissions."
  ),
  Prepare = c(
    "Install or update R and RStudio, review the opening chapter of FPP3, and work through the tsibble introduction before class.",
    "Read Chapter 2 of FPP3 and arrive ready to interpret time plots, season plots, lag plots, and ACF displays.",
    "Read Chapter 3 of FPP3 and come in ready to distinguish trend-cycle, seasonal, and remainder components.",
    "Read Chapter 5 plus Section 7.4 of FPP3 so the benchmark methods and useful predictors feel familiar before class.",
    "Read Sections 8.1 to 8.4 of FPP3 and review the difference between smoothing methods and their ETS models.",
    "Read Sections 8.3 to 8.7 of FPP3 with attention to seasonal ETS structure and automatic selection.",
    "Read Sections 9.1 to 9.2 of FPP3 and review the idea of stationarity before the workshop.",
    "Read Sections 9.3 to 9.8 of FPP3 and be ready to compare different ARIMA specifications.",
    "Read Sections 9.8 to 9.10 of FPP3 and focus on how seasonal ARIMA extends the earlier material.",
    "Read Chapter 7 of FPP3 and think about which predictors are genuinely available at forecast time.",
    "Read Chapter 10 of FPP3 and focus on how regression terms and ARIMA errors work together.",
    "Catch up on any unfinished exercises, review weak spots from earlier weeks, and prepare questions for revision."
  ),
  ResourceLabel = c(
    "FPP3 Chapter 1",
    "FPP3 Chapter 2",
    "FPP3 Chapter 3",
    "FPP3 Chapter 5",
    "FPP3 Chapter 8",
    "FPP3 Chapter 8",
    "FPP3 Chapter 9",
    "FPP3 Chapter 9",
    "FPP3 Chapter 9",
    "FPP3 Chapter 7",
    "FPP3 Chapter 10",
    "Textbook home"
  ),
  ResourceUrl = c(
    "https://otexts.com/fpp3/intro.html",
    "https://otexts.com/fpp3/graphics.html",
    "https://otexts.com/fpp3/decomposition.html",
    "https://otexts.com/fpp3/toolbox.html",
    "https://otexts.com/fpp3/expsmooth.html",
    "https://otexts.com/fpp3/expsmooth.html",
    "https://otexts.com/fpp3/arima.html",
    "https://otexts.com/fpp3/arima.html",
    "https://otexts.com/fpp3/arima.html",
    "https://otexts.com/fpp3/regression.html",
    "https://otexts.com/fpp3/dynamic.html",
    "https://otexts.com/fpp3/"
  ),
  Focus = list(
    c("Forecasting as a statistical decision problem", "What makes a series easy or hard to forecast", "Using tsibbles and the course R workflow"),
    c("Time plots, season plots, subseries plots, lag plots, and ACF plots", "Reading seasonal structure versus cycles", "Recognising white noise"),
    c("Transformations that stabilise variation", "Decomposition into trend-cycle, seasonal, and remainder parts", "Seasonal adjustment"),
    c("Benchmark forecasting methods", "Residuals, fitted values, and evaluation", "Linear trends, dummy seasonality, and transformations"),
    c("Simple exponential smoothing", "Trend methods and ETS notation", "Interpreting level and trend components"),
    c("Seasonal exponential smoothing", "ETS model classes", "Automatic model selection with the AICc"),
    c("Stationarity and differencing", "Random walks and persistence", "When ARIMA modelling becomes useful"),
    c("AR, MA, ARMA, and ARIMA structure", "Manual versus automatic order selection", "Forecast comparison across candidate models"),
    c("Seasonal ARIMA structure", "Generating and interpreting ARIMA forecasts", "Comparing ARIMA and ETS"),
    c("Useful predictors for forecasting", "Model selection in regression", "Ex ante versus ex post forecasting"),
    c("Dynamic regression with ARIMA errors", "Dynamic harmonic regression", "Lagged predictors and complex seasonality"),
    c("Revision of core forecasting methods", "Exam preparation", "Consolidating assignments and unresolved questions")
  )
)

course_weeks <- schedule |>
  filter(!is.na(Date), !is.na(Topic)) |>
  arrange(Date)

site_path <- function(path, depth = 0) {
  if (is.null(path) || identical(path, "") || is.na(path)) {
    return(NULL)
  }

  if (grepl("^(https?:)?//", path)) {
    return(path)
  }

  prefix <- if (depth > 0) paste(rep("../", depth), collapse = "") else "./"
  paste0(prefix, path)
}

render_focus_list <- function(items, class_name) {
  if (length(items) == 0) {
    return("<p class='muted'>No focus items listed yet.</p>")
  }

  paste0(
    "<ul class='", class_name, "'>",
    paste0("<li>", items, "</li>", collapse = ""),
    "</ul>"
  )
}

slide_asset_for_week <- function(week) {
  ann_file <- here::here(paste0("week", week, "/week_ann", week, ".pdf"))
  std_file <- here::here(paste0("week", week, "/week", week, ".pdf"))

  if (fs::file_exists(ann_file)) {
    paste0("week", week, "/week_ann", week, ".pdf")
  } else if (fs::file_exists(std_file)) {
    paste0("week", week, "/week", week, ".pdf")
  } else {
    NULL
  }
}

assessment_events <- bind_rows(
  assignments |>
    transmute(
      Date = Due,
      Label = Assignment,
      Kind = "Assignment",
      Url = paste0("assignments/", tools::file_path_sans_ext(File), ".html")
    ),
  quizzes |>
    transmute(
      Date = QDue,
      Label = paste0(Quiz, " quiz"),
      Kind = "Quiz",
      Url = NA_character_
    )
) |>
  arrange(Date)

format_upcoming_assessment <- function(start_date, end_date = start_date + 7) {
  events <- assessment_events |>
    filter(Date >= start_date, Date < end_date) |>
    arrange(Date)

  if (NROW(events) > 0) {
    paste(
      paste0(events$Label, " on ", format(events$Date, "%A %d %B")),
      collapse = " · "
    )
  } else {
    next_event <- assessment_events |>
      filter(Date >= start_date) |>
      slice_head(n = 1)

    if (NROW(next_event) > 0) {
      paste0(
        "Next assessment: ",
        next_event$Label[[1]],
        " on ",
        format(next_event$Date[[1]], "%A %d %B")
      )
    } else {
      "No remaining assessment deadlines are listed in the schedule."
    }
  }
}

get_current_week_context <- function(reference_date = Sys.Date()) {
  today <- as.Date(reference_date)
  current <- course_weeks |>
    filter(today >= Date, today < Date + 7)

  if (NROW(current) == 0) {
    if (today < min(course_weeks$Date)) {
      current <- course_weeks |>
        slice_head(n = 1)
    } else {
      current <- course_weeks |>
        filter(Date <= today) |>
        slice_tail(n = 1)
    }
  }

  next_week <- course_weeks |>
    filter(!is.na(Week), Date > current$Date) |>
    slice_head(n = 1)

  details <- tibble()
  if (!is.na(current$Week)) {
    details <- week_metadata |>
      filter(Week == current$Week)
  }

  list(
    today = today,
    current = current,
    details = details,
    next_week = next_week
  )
}

show_week_dashboard <- function(reference_date = Sys.Date()) {
  context <- get_current_week_context(reference_date)
  current <- context$current
  details <- context$details
  next_week <- context$next_week
  is_break <- is.na(current$Week)

  heading <- if (is_break) {
    "Mid-semester break"
  } else {
    paste0("Week ", current$Week, " · ", current$Topic)
  }

  summary <- if (is_break) {
    "There is no regular teaching this week. Use the break to catch up on recordings, revise earlier material, and get ahead on the next topic."
  } else {
    details$Summary[[1]]
  }

  prepare <- if (is_break) {
    "Use the week to consolidate earlier material and queue up the next chapter before classes resume."
  } else {
    details$Prepare[[1]]
  }

  workshop <- if (is_break) {
    "No seminar or workshop this week."
  } else {
    paste0(
      format(current$Date + 2, "%A %d %B"),
      " · Seminar 3:00pm to 3:50pm, workshop 4:00pm to 4:50pm · Building K Level 3, K321"
    )
  }

  next_up <- if (NROW(next_week) == 0) {
    "Final teaching week reached."
  } else {
    paste0(
      "Week ",
      next_week$Week,
      " starts ",
      format(next_week$Date + 2, "%d %b"),
      ": ",
      next_week$Topic
    )
  }

  assessment_text <- format_upcoming_assessment(current$Date)
  week_page <- if (is_break && NROW(next_week) > 0) {
    site_path(paste0("week", next_week$Week, "/index.html"))
  } else if (is_break) {
    "./index.html"
  } else {
    site_path(paste0("week", current$Week, "/index.html"))
  }

  week_page_label <- if (is_break) "Preview next teaching week" else "Open week page"

  slides_url <- if (!is_break) {
    site_path(slide_asset_for_week(current$Week))
  } else {
    NULL
  }

  resource_url <- if (!is_break) {
    details$ResourceUrl[[1]]
  } else {
    site_path("assignments/A2.html")
  }
  resource_label <- if (!is_break) details$ResourceLabel[[1]] else "Assessments"
  focus_html <- if (is_break) {
    paste0("<p>", next_up, "</p>")
  } else {
    render_focus_list(details$Focus[[1]], "week-dashboard__focus-list")
  }

  html <- c(
    "<div class='week-dashboard'>",
    "<div class='week-dashboard__header'>",
    if (is_break) "<span class='week-dashboard__status week-dashboard__status--break'>Break week</span>" else "<span class='week-dashboard__status'>This week</span>",
    paste0("<h2>", heading, "</h2>"),
    paste0("<p>", summary, "</p>"),
    "</div>",
    "<div class='week-dashboard__grid'>",
    "<div class='week-dashboard__card'>",
    "<span class='week-dashboard__label'>Prepare before class</span>",
    paste0("<p>", prepare, "</p>"),
    "</div>",
    "<div class='week-dashboard__card'>",
    "<span class='week-dashboard__label'>Seminar and workshop</span>",
    paste0("<p>", workshop, "</p>"),
    "</div>",
    "<div class='week-dashboard__card'>",
    if (is_break) "<span class='week-dashboard__label'>After the break</span>" else "<span class='week-dashboard__label'>Key focus</span>",
    focus_html,
    "</div>",
    "<div class='week-dashboard__card'>",
    "<span class='week-dashboard__label'>Assessment</span>",
    paste0("<p>", assessment_text, "</p>"),
    "</div>",
    "</div>",
    if (!is_break) paste0("<p class='week-dashboard__next'>Coming up next: ", next_up, "</p>"),
    "<div class='week-dashboard__actions'>",
    paste0("<a class='button-link' href='", week_page, "'>", week_page_label, "</a>"),
    if (!is.null(slides_url)) paste0("<a class='button-link button-link--ghost' href='", slides_url, "'>Slides PDF</a>"),
    paste0("<a class='button-link button-link--ghost' href='", resource_url, "'>", resource_label, "</a>"),
    "<a class='button-link button-link--ghost' href='https://echo360.net.au/section/630d3bbb-85b4-47c8-842d-4bafc932413d/home'>Recordings</a>",
    "</div>",
    "</div>"
  )

  cat(paste(html, collapse = ""))
}

show_week_overview <- function(week, depth = 1) {
  details <- week_metadata |>
    filter(Week == week)

  this_week <- course_weeks |>
    filter(Week == week) |>
    slice_head(n = 1)

  assessment_text <- format_upcoming_assessment(this_week$Date, this_week$Date + 7)
  slides_url <- site_path(slide_asset_for_week(week), depth)
  resource_url <- details$ResourceUrl[[1]]

  html <- c(
    "<div class='week-overview'>",
    "<div class='week-overview__header'>",
    paste0("<span class='week-overview__status'>Week ", week, "</span>"),
    paste0("<p>", details$Summary[[1]], "</p>"),
    "</div>",
    "<div class='week-overview__grid'>",
    "<div class='week-overview__card'>",
    "<span class='week-overview__label'>Prepare before class</span>",
    paste0("<p>", details$Prepare[[1]], "</p>"),
    "</div>",
    "<div class='week-overview__card'>",
    "<span class='week-overview__label'>Seminar and workshop</span>",
    paste0("<p>", format(this_week$Date + 2, "%A %d %B"), " · Seminar 3:00pm to 3:50pm, workshop 4:00pm to 4:50pm · Building K Level 3, K321</p>"),
    "</div>",
    "<div class='week-overview__card'>",
    "<span class='week-overview__label'>Focus</span>",
    render_focus_list(details$Focus[[1]], "week-overview__focus-list"),
    "</div>",
    "<div class='week-overview__card'>",
    "<span class='week-overview__label'>Assessment</span>",
    paste0("<p>", assessment_text, "</p>"),
    "</div>",
    "</div>",
    "<div class='week-overview__actions'>",
    if (!is.null(slides_url)) paste0("<a class='button-link' href='", slides_url, "'>Slides PDF</a>"),
    paste0("<a class='button-link button-link--ghost' href='", resource_url, "'>", details$ResourceLabel[[1]], "</a>"),
    "<a class='button-link button-link--ghost' href='https://echo360.net.au/section/630d3bbb-85b4-47c8-842d-4bafc932413d/home'>Recordings</a>",
    "</div>",
    "</div>"
  )

  cat(paste(html, collapse = ""))
}

show_assignments <- function(week) {
  ass <- schedule |>
    filter(
      Week >= week,
      !is.na(Assignment),
    ) |>
    filter(Week == min(Week) | Week - week <= 2) |>
    select(Assignment:File)
  if(NROW(ass) > 0) {
    cat("\n\n## Assignments\n\n")
    for(i in seq(NROW(ass))) {
      cat("* [", ass$Assignment[i], "](../", ass$File[i], ") is due on ",
          format(ass$Due[i], "%A %d %B.\n"), sep="")
    }
  }
  show_quiz(week)
}

show_quiz <- function(week){
  ass <- schedule %>%
    filter(Week == week, !is.na(Quiz)) %>%
    select(Quiz, QDue, QMoodle3231, QMoodle5231)

  if (NROW(ass) > 0) {
    cat("\n\n## Weekly quiz\n\n")

    for (i in seq_len(NROW(ass))) {

      cat("* ", ass$Quiz[i], " quiz: ",
          "[ETF3231](", ass$QMoodle3231[i], ") | ",
          "[ETF5231](", ass$QMoodle5231[i], ")",
          " — Due ",
          format(ass$QDue[i], "%A %d %B."),
          "\n",
          sep = ""
      )
    }
  }
}

show_slides <- function(week) {
  file <- paste0("week", week, ".pdf")
  embed <- paste0(
      "<embed src='",
      file,
      "' type='application/pdf' width='100%' height=465></embed>"
    )
  button <- paste0("<a href=", file, " class='badge badge-small badge-red'>Download pdf</a>")
  cat(paste0("## Slides for seminar\n\n", embed,"\n", button))
}


show_slides_ann <- function(week) {
  file <- paste0("week_ann", week, ".pdf")
  embed <- paste0(
    "<embed src='",
    file,
    "' type='application/pdf' width='100%' height=465></embed>"
  )
  button <- paste0("<a href=", file, " class='badge badge-small badge-red'>Download pdf</a>")
  cat(paste0("## Slides for seminar\n\n", embed,"\n", button))
}

show_activity <- function(week, title = TRUE, show_solutions = TRUE) {
  today <- Sys.Date()
  monday <- monday <- schedule |>
    filter(Week == week) |>
    pull(Date) |>
    as.Date()
  # Show slides one week ahead
  if ((monday - today) <= 7 | week <= 1) {
    file <- here::here(paste0("week", week, "/activities.qmd"))
    if (fs::file_exists(file)) {
      cat("\n\n## [Workshop activities](activities.qmd)\n\n")
    }
  }
}

#
#
# show_activity <- function(week, title = TRUE) {
#   file <- here::here(paste0("week",week,"/activities.qmd"))
#   if(!fs::file_exists(file)) {
#     file <- here::here(paste0("week",week,"/activities.md"))
#   }
#   activities <- read_file(file)
#   if(title) {
#     cat("\n\n## Seminar activities\n\n")
#   }
#   cat(activities)
#   cat("\n")
# }

submit <- function(schedule, assignment) {
  ass <- schedule  |>
    filter(Assignment == !!assignment)
  due <- format(ass$Due, "%e %B %Y") |> stringr::str_trim()
  url <- ass$Moodle
  button1 <- paste0(
    "<div class='assignment-cta'>",
    "<div>",
    "<p class='assignment-cta__label'>Submission</p>",
    "<p class='assignment-cta__due'>Due ", due, "</p>",
    "<p class='assignment-cta__text'>Open the Moodle submission page for instructions, links and final upload details.</p>",
    "</div>",
    "<a href='", url, "' class='button-link assignment-cta__button'>Open ETF5231 submission</a>",
    "</div>"
  )
  cat(button1)
  if (str_detect(ass$Assignment, "IA")) {
  url <- ass$Moodle3231
  button2 <- paste0(
    "<div class='resource-actions'>",
    "<a href='", url, "' class='button-link button-link--ghost'>Open ETF3231 submission</a>",
    "</div>"
  )
  cat(button2)
  }
}
