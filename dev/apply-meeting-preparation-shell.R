manifest_path <- "dev/course-manifest.csv"

content_begin <-
  "<!-- BEGIN GENERATED MEETING PREPARATION CONTENT -->"

content_end <-
  "<!-- END GENERATED MEETING PREPARATION CONTENT -->"

navigation_end <-
  "<!-- END GENERATED MODULE NAVIGATION -->"


# Read and validate --------------------------------------------------------

manifest <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pages <- manifest[
  manifest$page_type == "readiness",
  ,
  drop = FALSE
]

pages <- pages[
  order(pages$module_number, pages$page_order),
]

stopifnot(
  nrow(pages) == 7,
  all(
    pages$page_title ==
      "Hogyan készülj a meetingre?"
  )
)

missing_pages <- pages$relative_path[
  !file.exists(pages$relative_path)
]

if (length(missing_pages) > 0) {
  stop(
    "Missing meeting-preparation pages:\n",
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
    yaml = lines[
      2:(yaml_end - 1)
    ],
    
    body = if (
      yaml_end < length(lines)
    ) {
      lines[
        (yaml_end + 1):length(lines)
      ]
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
    grepl(
      pattern,
      yaml
    )
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
    !grepl(
      pattern,
      yaml
    )
  ]
}

is_top_level_yaml_key <- function(line) {
  grepl(
    "^[A-Za-z0-9_-]+:",
    line
  )
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
    stop(
      "Multiple grid blocks found."
    )
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
      vapply(
        yaml[following],
        is_top_level_yaml_key,
        logical(1)
      )
    ]
    
    grid_end <- if (
      length(next_key) > 0
    ) {
      next_key[[1]] - 1
    } else {
      length(yaml)
    }
  }
  
  grid_positions <- seq.int(
    grid_start,
    grid_end
  )
  
  body_width_position <- grid_positions[
    grepl(
      "^\\s+body-width\\s*:",
      yaml[grid_positions]
    )
  ]
  
  if (length(body_width_position) > 1) {
    stop(
      "Multiple body-width values found."
    )
  }
  
  if (length(body_width_position) == 1) {
    yaml[body_width_position] <- paste0(
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

meeting_preparation_content <- function() {
  c(
    content_begin,
    "",
    paste0(
      "Gratulálunk, elértél a modul végéhez. ",
      "Most már minden eszköz a rendelkezésedre áll ahhoz, ",
      "hogy a projektetek a következő fázisba lépjen. ",
      "Ezen az oldalon összegyűjtöttünk néhány gyakorlati ",
      "tanácsot, amelyek segíthetnek abban, hogy a személyes ",
      "meeting gördülékenyen és eredményesen menjen."
    ),
    "",
    "::: {.meeting-preparation-panel}",
    paste0(
      '<div class="meeting-preparation-panel__title">',
      "A meeting előtt hasznos lehet, ha…",
      "</div>"
    ),
    "",
    "- *Az első, modulhoz kapcsolódó gyakorlati tanács helye.*",
    "- *A második, modulhoz kapcsolódó gyakorlati tanács helye.*",
    "- *A harmadik, modulhoz kapcsolódó gyakorlati tanács helye.*",
    ":::",
    "",
    content_end
  )
}

replace_or_create_content <- function(
    body,
    path
) {
  begin_position <- which(
    body == content_begin
  )
  
  end_position <- which(
    body == content_end
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
        "Invalid meeting-preparation markers in: ",
        path
      )
    }
    
    before <- if (
      begin_position > 1
    ) {
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
        before,
        meeting_preparation_content(),
        after
      )
    )
  }
  
  nav_end_position <- which(
    body == navigation_end
  )
  
  if (length(nav_end_position) != 1) {
    stop(
      "Generated module navigation not found in: ",
      path
    )
  }
  
  nav_end_position <- nav_end_position[[1]]
  
  old_content <- if (
    nav_end_position < length(body)
  ) {
    body[
      (nav_end_position + 1):length(body)
    ]
  } else {
    character()
  }
  
  has_known_placeholder <- any(
    grepl(
      paste0(
        "Ez az oldal a navigációs ",
        "prototípus része"
      ),
      old_content,
      fixed = TRUE
    )
  )
  
  if (!has_known_placeholder) {
    stop(
      paste0(
        "The page no longer contains the known placeholder body. ",
        "Refusing to overwrite content in: ",
        path
      )
    )
  }
  
  c(
    body[
      seq_len(nav_end_position)
    ],
    "",
    meeting_preparation_content(),
    ""
  )
}


# Apply page shell ---------------------------------------------------------

updated_pages <- character()

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
  
  body <- replace_or_create_content(
    body = parts$body,
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
}


# Validation ---------------------------------------------------------------

for (path in updated_pages) {
  lines <- read_utf8(path)
  
  parts <- split_front_matter(
    lines,
    path
  )
  
  stopifnot(
    any(
      grepl(
        paste0(
          '^title\\s*:\\s*"',
          "Hogyan készülj a meetingre\\?",
          '"\\s*$'
        ),
        parts$yaml
      )
    ),
    
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
    
    sum(parts$body == content_begin) == 1,
    sum(parts$body == content_end) == 1,
    
    !any(
      grepl(
        "- [ ]",
        parts$body,
        fixed = TRUE
      )
    ),
    
    !any(
      trimws(parts$body) ==
        "## Önellenőrzés"
    )
  )
}

message(
  "Meeting-preparation shell applied successfully to ",
  length(updated_pages),
  " pages."
)