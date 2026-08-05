manifest <- read.csv(
  "dev/course-manifest.csv",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

overview_paths <- manifest$relative_path[
  manifest$page_type == "overview"
]

set_body_class <- function(
    yaml,
    class_name = "module-content-page"
) {
  position <- which(
    grepl(
      "^body-classes\\s*:",
      yaml
    )
  )
  
  if (length(position) > 1) {
    stop("Multiple body-classes fields found in YAML.")
  }
  
  value <- paste0(
    "body-classes: ",
    class_name
  )
  
  if (length(position) == 1) {
    yaml[position] <- value
  } else {
    yaml <- c(
      yaml,
      "",
      value
    )
  }
  
  yaml
}

set_overview_width <- function(
    path,
    width = "1000px"
) {
  lines <- readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
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
  
  yaml <- lines[
    2:(yaml_end - 1)
  ]
  
  body <- if (yaml_end < length(lines)) {
    lines[
      (yaml_end + 1):length(lines)
    ]
  } else {
    character()
  }
  
  grid_position <- which(
    grepl(
      "^grid\\s*:\\s*$",
      yaml
    )
  )
  
  if (length(grid_position) > 1) {
    stop(
      "Multiple grid blocks found in: ",
      path
    )
  }
  
  if (length(grid_position) == 0) {
    yaml <- c(
      yaml,
      "",
      "grid:",
      paste0(
        "  body-width: ",
        width
      )
    )
  } else {
    grid_start <- grid_position[[1]]
    
    following <- seq.int(
      grid_start + 1,
      length(yaml)
    )
    
    next_top_level <- following[
      grepl(
        "^[A-Za-z0-9_-]+:",
        yaml[following]
      )
    ]
    
    grid_end <- if (
      length(next_top_level) > 0
    ) {
      next_top_level[[1]] - 1
    } else {
      length(yaml)
    }
    
    grid_block_positions <- seq.int(
      grid_start,
      grid_end
    )
    
    body_width_position <- grid_block_positions[
      grepl(
        "^\\s+body-width\\s*:",
        yaml[grid_block_positions]
      )
    ]
    
    if (length(body_width_position) > 1) {
      stop(
        "Multiple body-width values found in: ",
        path
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
  }
  
  yaml <- set_body_class(
    yaml,
    class_name = "module-content-page"
  )
  
  writeLines(
    c(
      "---",
      yaml,
      "---",
      body
    ),
    con = path,
    useBytes = TRUE
  )
}


for (path in overview_paths) {
  set_overview_width(path)
}

message(
  "Overview width applied to ",
  length(overview_paths),
  " module pages."
)