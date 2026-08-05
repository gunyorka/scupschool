manifest_path <- "dev/course-manifest.csv"

manifest <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

core_pages <- manifest[
  manifest$page_type == "core",
  ,
  drop = FALSE
]

missing_pages <- core_pages$relative_path[
  !file.exists(core_pages$relative_path)
]

if (length(missing_pages) > 0) {
  stop(
    "Missing core pages:\n",
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

split_front_matter <- function(lines, path) {
  delimiters <- which(trimws(lines) == "---")
  
  if (
    length(delimiters) < 2 ||
    delimiters[[1]] != 1
  ) {
    stop("Valid YAML front matter not found in: ", path)
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

is_top_level_yaml_key <- function(line) {
  grepl(
    "^[A-Za-z0-9_-]+:",
    line
  )
}

remove_subtitle <- function(yaml) {
  yaml[
    !grepl(
      "^subtitle\\s*:",
      yaml
    )
  ]
}

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
    stop("Multiple grid blocks found in YAML.")
  }
  
  if (length(grid_position) == 0) {
    return(
      c(
        yaml,
        "",
        "grid:",
        paste0("  body-width: ", width)
      )
    )
  }
  
  grid_start <- grid_position[[1]]
  
  following_positions <- seq.int(
    grid_start + 1,
    length(yaml)
  )
  
  if (length(following_positions) == 0) {
    grid_end <- length(yaml)
  } else {
    next_key <- following_positions[
      vapply(
        yaml[following_positions],
        is_top_level_yaml_key,
        logical(1)
      )
    ]
    
    grid_end <- if (length(next_key) > 0) {
      next_key[[1]] - 1
    } else {
      length(yaml)
    }
  }
  
  grid_block <- yaml[
    seq.int(grid_start, grid_end)
  ]
  
  body_width_position <- which(
    grepl(
      "^\\s+body-width\\s*:",
      grid_block
    )
  )
  
  if (length(body_width_position) > 1) {
    stop("Multiple body-width values found in grid block.")
  }
  
  if (length(body_width_position) == 1) {
    grid_block[body_width_position] <-
      paste0("  body-width: ", width)
  } else {
    grid_block <- append(
      grid_block,
      paste0("  body-width: ", width),
      after = 1
    )
  }
  
  c(
    if (grid_start > 1) {
      yaml[seq_len(grid_start - 1)]
    } else {
      character()
    },
    grid_block,
    if (grid_end < length(yaml)) {
      yaml[(grid_end + 1):length(yaml)]
    } else {
      character()
    }
  )
}


# Apply shell --------------------------------------------------------------

updated_pages <- character()

for (path in core_pages$relative_path) {
  lines <- read_utf8(path)
  
  parts <- split_front_matter(
    lines,
    path
  )
  
  yaml <- remove_subtitle(parts$yaml)
  
  yaml <- set_body_width(
    yaml,
    width = "1000px"
  )
  
  yaml <- set_body_class(
    yaml,
    class_name = "module-content-page"
  )
  
  updated <- c(
    "---",
    yaml,
    "---",
    parts$body
  )
  
  write_utf8(
    updated,
    path
  )
  
  updated_pages <- c(
    updated_pages,
    path
  )
}


# Validation ---------------------------------------------------------------

validation <- lapply(
  updated_pages,
  function(path) {
    lines <- read_utf8(path)
    parts <- split_front_matter(lines, path)
    
    data.frame(
      path = path,
      
      has_subtitle = any(
        grepl(
          "^subtitle\\s*:",
          parts$yaml
        )
      ),
      
      has_body_width = any(
        grepl(
          "^\\s+body-width\\s*:\\s*1000px\\s*$",
          parts$yaml
        )
      ),
      
      has_body_class = any(
        grepl(
          "^body-classes\\s*:\\s*module-content-page\\s*$",
          parts$yaml
        )
      ),
      
      stringsAsFactors = FALSE
    )
  }
)

validation <- do.call(
  rbind,
  validation
)

if (any(validation$has_subtitle)) {
  stop("At least one core page still contains a subtitle.")
}

if (!all(validation$has_body_width)) {
  stop("At least one core page is missing body-width: 1000px.")
}

if (!all(validation$has_body_class)) {
  stop(
    "At least one core page is missing ",
    "body-classes: module-content-page."
  )
}
message(
  "Core-page shell applied successfully to ",
  length(updated_pages),
  " pages."
)