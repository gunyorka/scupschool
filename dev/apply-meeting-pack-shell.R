manifest_path <- "dev/course-manifest.csv"

navigation_end <-
  "<!-- END GENERATED MODULE NAVIGATION -->"

support_begin <-
  "<!-- BEGIN GENERATED MEETING SUPPORT -->"

support_end <-
  "<!-- END GENERATED MEETING SUPPORT -->"


# Read and validate --------------------------------------------------------

manifest <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pages <- manifest[
  manifest$page_type == "meeting_pack",
  ,
  drop = FALSE
]

pages <- pages[
  order(pages$module_number, pages$page_order),
]

stopifnot(nrow(pages) == 7)

missing_pages <- pages$relative_path[
  !file.exists(pages$relative_path)
]

if (length(missing_pages) > 0) {
  stop(
    "Missing Meeting pack pages:\n",
    paste(missing_pages, collapse = "\n")
  )
}


# Helpers -----------------------------------------------------------------

read_utf8 <- function(path) {
  readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )
}

write_utf8 <- function(lines, path) {
  writeLines(
    lines,
    con = path,
    useBytes = TRUE
  )
}

yaml_string <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  
  paste0('"', x, '"')
}

split_front_matter <- function(lines, path) {
  delimiters <- which(
    trimws(lines) == "---"
  )
  
  if (
    length(delimiters) < 2 ||
    delimiters[[1]] != 1
  ) {
    stop(
      "Valid YAML front matter not found in: ",
      path
    )
  }
  
  yaml_end <- delimiters[[2]]
  
  list(
    yaml = lines[2:(yaml_end - 1)],
    
    body = if (yaml_end < length(lines)) {
      lines[(yaml_end + 1):length(lines)]
    } else {
      character()
    }
  )
}

set_yaml_value <- function(
    yaml,
    key,
    value
) {
  pattern <- paste0(
    "^",
    key,
    "\\s*:"
  )
  
  position <- which(
    grepl(pattern, yaml)
  )
  
  if (length(position) > 1) {
    stop(
      "Multiple YAML fields found for: ",
      key
    )
  }
  
  new_line <- paste0(
    key,
    ": ",
    value
  )
  
  if (length(position) == 1) {
    yaml[position] <- new_line
  } else {
    yaml <- c(
      yaml,
      "",
      new_line
    )
  }
  
  yaml
}

remove_yaml_value <- function(
    yaml,
    key
) {
  pattern <- paste0(
    "^",
    key,
    "\\s*:"
  )
  
  yaml[
    !grepl(pattern, yaml)
  ]
}

set_body_width <- function(
    yaml,
    width = "1000px"
) {
  grid_position <- which(
    grepl(
      "^grid\\s*:\\s*$",
      yaml
    )
  )
  
  if (length(grid_position) > 1) {
    stop("Multiple grid blocks found.")
  }
  
  if (length(grid_position) == 0) {
    return(
      c(
        yaml,
        "",
        "grid:",
        paste0(
          "  body-width: ",
          width
        )
      )
    )
  }
  
  grid_start <- grid_position[[1]]
  
  following <- seq.int(
    grid_start + 1,
    length(yaml)
  )
  
  if (length(following) == 0) {
    grid_end <- length(yaml)
  } else {
    next_key <- following[
      grepl(
        "^[A-Za-z0-9_-]+:",
        yaml[following]
      )
    ]
    
    grid_end <- if (length(next_key) > 0) {
      next_key[[1]] - 1
    } else {
      length(yaml)
    }
  }
  
  grid_positions <- seq.int(
    grid_start,
    grid_end
  )
  
  width_position <- grid_positions[
    grepl(
      "^\\s+body-width\\s*:",
      yaml[grid_positions]
    )
  ]
  
  if (length(width_position) > 1) {
    stop("Multiple body-width values found.")
  }
  
  if (length(width_position) == 1) {
    yaml[width_position] <- paste0(
      "  body-width: ",
      width
    )
  } else {
    yaml <- append(
      yaml,
      paste0(
        "  body-width: ",
        width
      ),
      after = grid_start
    )
  }
  
  yaml
}

trim_trailing_blank_lines <- function(lines) {
  while (
    length(lines) > 0 &&
    !nzchar(trimws(lines[[length(lines)]]))
  ) {
    lines <- lines[-length(lines)]
  }
  
  lines
}


# Placeholder for Meeting packs without authored content -------------------

meeting_pack_placeholder <- function() {
  c(
    "## A meeting célja",
    "",
    paste0(
      "*[A meeting céljának rövid, ",
      "modulspecifikus bemutatása kerül ide.]*"
    ),
    "",
    "- *[Az első ajánlott cél helye.]*",
    "- *[A második ajánlott cél helye.]*",
    "- *[A harmadik ajánlott cél helye.]*",
    "",
    "## Mire lesz szükségetek?",
    "",
    "- *[Szükséges vagy hasznos eszköz helye.]*",
    "- *[Szükséges dokumentum vagy tananyag helye.]*",
    "- *[Technikai vagy gyakorlati feltétel helye.]*",
    "",
    "## Ajánlott agenda",
    "",
    paste0(
      "Az alábbi időkeretek ajánlások. ",
      "Alakítsátok őket a csapatotok munkamódjához és ",
      "a meeting céljához."
    ),
    "",
    ":::: {.meeting-agenda}",
    "",
    "::: {.meeting-agenda__item}",
    "[0–10 perc]{.meeting-agenda__time}",
    "",
    "### Megérkezés és közös fókusz",
    "",
    "*[A meeting megnyitásának rövid leírása.]*",
    ":::",
    "",
    "::: {.meeting-agenda__item}",
    "[10–30 perc]{.meeting-agenda__time}",
    "",
    "### Első munkaszakasz",
    "",
    "*[Az első közös munkaszakasz leírása.]*",
    ":::",
    "",
    "::: {.meeting-agenda__item}",
    "[30–60 perc]{.meeting-agenda__time}",
    "",
    "### Közös feldolgozás és döntések",
    "",
    "*[A meeting fő munkaszakaszának leírása.]*",
    ":::",
    "",
    "::: {.meeting-agenda__item}",
    "[60–75 perc]{.meeting-agenda__time}",
    "",
    "### Lezárás és következő lépések",
    "",
    "*[A meeting lezárásának rövid leírása.]*",
    ":::",
    "",
    "::::",
    "",
    "## Tippek a meetingre",
    "",
    "::: {.learning-block .learning-block--tip}",
    '<div class="learning-block__title">Tipp</div>',
    "",
    "- *[Az első modulspecifikus meetingtipp helye.]*",
    "- *[A második modulspecifikus meetingtipp helye.]*",
    "- *[A harmadik modulspecifikus meetingtipp helye.]*",
    ":::"
  )
}


# Standard support section -------------------------------------------------

meeting_support_block <- function() {
  c(
    support_begin,
    "",
    "## További segítség",
    "",
    "::::: {.meeting-support}",
    "",
    paste0(
      "A személyes meetingek vezetéséhez és a csapaton belüli ",
      "helyzetek kezeléséhez a Segítségtár két kisokosa ad ",
      "további, opcionális segítséget. Akkor nyissátok meg őket, ",
      "amikor egy konkrét helyzetben részletesebb tanácsra van ",
      "szükségetek."
    ),
    "",
    ":::: {.meeting-support__links}",
    "",
    "::: {.meeting-support__link}",
    paste0(
      "### [Személyes meeting kisokos]",
      "(../../support/szemelyes-meeting-kisokos/index.qmd)"
    ),
    "",
    paste0(
      "Gyakorlati tanácsok a meeting előkészítéséhez, ",
      "fókuszált vezetéséhez és egyértelmű lezárásához."
    ),
    ":::",
    "",
    "::: {.meeting-support__link}",
    paste0(
      "### [Csapatdinamika kisokos]",
      "(../../support/csapatdinamika-kisokos/index.qmd)"
    ),
    "",
    paste0(
      "Segítség a közös döntésekhez, az eltérő vélemények ",
      "kezeléséhez és a kiegyensúlyozott részvételhez."
    ),
    ":::",
    "",
    "::::",
    "",
    ":::::",
    "",
    support_end
  )
}


# Preserve authored content; replace only known placeholders ---------------

replace_known_placeholder <- function(
    body,
    path
) {
  navigation_position <- which(
    body == navigation_end
  )
  
  if (length(navigation_position) != 1) {
    stop(
      "Generated module navigation not found in: ",
      path
    )
  }
  
  navigation_position <-
    navigation_position[[1]]
  
  content <- if (
    navigation_position < length(body)
  ) {
    body[
      (navigation_position + 1):length(body)
    ]
  } else {
    character()
  }
  
  has_known_placeholder <- any(
    grepl(
      "Ez az oldal a navigációs prototípus része",
      content,
      fixed = TRUE
    )
  )
  
  if (!has_known_placeholder) {
    return(body)
  }
  
  c(
    body[
      seq_len(navigation_position)
    ],
    "",
    meeting_pack_placeholder(),
    ""
  )
}


# Insert or update the standard support section ----------------------------

set_support_block <- function(
    body,
    path
) {
  begin_position <- which(
    body == support_begin
  )
  
  end_position <- which(
    body == support_end
  )
  
  if (
    length(begin_position) > 0 ||
    length(end_position) > 0
  ) {
    if (
      length(begin_position) != 1 ||
      length(end_position) != 1 ||
      begin_position >= end_position
    ) {
      stop(
        "Invalid Meeting support markers in: ",
        path
      )
    }
    
    before <- if (begin_position > 1) {
      body[
        seq_len(begin_position - 1)
      ]
    } else {
      character()
    }
    
    after <- if (
      end_position < length(body)
    ) {
      body[
        (end_position + 1):length(body)
      ]
    } else {
      character()
    }
    
    return(
      c(
        trim_trailing_blank_lines(before),
        "",
        meeting_support_block(),
        after
      )
    )
  }
  
  support_heading <- which(
    trimws(body) == "## További segítség"
  )
  
  if (length(support_heading) > 1) {
    stop(
      "Multiple További segítség headings found in: ",
      path
    )
  }
  
  if (length(support_heading) == 1) {
    # The support section is the final section in the approved template.
    before <- if (
      support_heading[[1]] > 1
    ) {
      body[
        seq_len(support_heading[[1]] - 1)
      ]
    } else {
      character()
    }
    
    return(
      c(
        trim_trailing_blank_lines(before),
        "",
        meeting_support_block()
      )
    )
  }
  
  c(
    trim_trailing_blank_lines(body),
    "",
    meeting_support_block()
  )
}


# Apply the shell ----------------------------------------------------------

updated_pages <- character()
generated_placeholders <- character()
preserved_pages <- character()

for (i in seq_len(nrow(pages))) {
  row <- pages[
    i,
    ,
    drop = FALSE
  ]
  
  path <- row$relative_path[[1]]
  
  lines <- read_utf8(path)
  
  parts <- split_front_matter(
    lines,
    path
  )
  
  yaml <- set_yaml_value(
    parts$yaml,
    key = "title",
    value = yaml_string(
      row$page_title[[1]]
    )
  )
  
  yaml <- remove_yaml_value(
    yaml,
    key = "subtitle"
  )
  
  yaml <- set_body_width(
    yaml,
    width = "1000px"
  )
  
  yaml <- set_yaml_value(
    yaml,
    key = "body-classes",
    value = "module-content-page"
  )
  
  had_placeholder <- any(
    grepl(
      "Ez az oldal a navigációs prototípus része",
      parts$body,
      fixed = TRUE
    )
  )
  
  body <- replace_known_placeholder(
    body = parts$body,
    path = path
  )
  
  body <- set_support_block(
    body = body,
    path = path
  )
  
  write_utf8(
    c(
      "---",
      yaml,
      "---",
      body
    ),
    path
  )
  
  updated_pages <- c(
    updated_pages,
    path
  )
  
  if (had_placeholder) {
    generated_placeholders <- c(
      generated_placeholders,
      path
    )
  } else {
    preserved_pages <- c(
      preserved_pages,
      path
    )
  }
}


# Validation ---------------------------------------------------------------

for (path in updated_pages) {
  lines <- read_utf8(path)
  
  parts <- split_front_matter(
    lines,
    path
  )
  
  stopifnot(
    !any(
      grepl(
        "^subtitle\\s*:",
        parts$yaml
      )
    ),
    
    any(
      grepl(
        "^\\s+body-width\\s*:\\s*1000px\\s*$",
        parts$yaml
      )
    ),
    
    any(
      grepl(
        paste0(
          "^body-classes\\s*:\\s*",
          "module-content-page\\s*$"
        ),
        parts$yaml
      )
    ),
    
    sum(parts$body == support_begin) == 1,
    sum(parts$body == support_end) == 1,
    
    any(
      trimws(parts$body) ==
        "## A meeting célja"
    ),
    
    any(
      trimws(parts$body) ==
        "## Mire lesz szükségetek?"
    ),
    
    any(
      trimws(parts$body) ==
        "## Ajánlott agenda"
    ),
    
    any(
      trimws(parts$body) ==
        "## Tippek a meetingre"
    ),
    
    !any(
      grepl(
        "Ez az oldal a navigációs prototípus része",
        parts$body,
        fixed = TRUE
      )
    )
  )
}

message(
  "Meeting pack shell applied successfully to ",
  length(updated_pages),
  " pages."
)

message(
  "Generated placeholder structure on ",
  length(generated_placeholders),
  " pages."
)

message(
  "Preserved existing authored content on ",
  length(preserved_pages),
  " pages."
)