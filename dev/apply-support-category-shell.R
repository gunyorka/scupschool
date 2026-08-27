# Canonical support-category shell -----------------------------------------

manifest_path <- "dev/support-manifest.csv"


# Read manifest ------------------------------------------------------------

manifest <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

categories <- manifest[
  manifest$item_type == "category",
  ,
  drop = FALSE
]

categories <- categories[
  order(categories$category_order),
  ,
  drop = FALSE
]

if (nrow(categories) == 0) {
  stop("No support categories found in the manifest.")
}

if (anyDuplicated(categories$relative_path)) {
  stop("Duplicate support-category paths found.")
}

missing_categories <- categories$relative_path[
  !file.exists(categories$relative_path)
]

if (length(missing_categories) > 0) {
  stop(
    "Missing support-category pages:\n",
    paste(missing_categories, collapse = "\n")
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

yaml_scalar <- function(
    yaml,
    key
) {
  pattern <- paste0(
    "^",
    key,
    "\\s*:"
  )
  
  hits <- grep(
    pattern,
    yaml,
    value = TRUE
  )
  
  if (length(hits) != 1) {
    return(NA_character_)
  }
  
  value <- sub(
    pattern,
    "",
    hits
  )
  
  value <- trimws(value)
  
  if (
    nchar(value) >= 2 &&
    substr(value, 1, 1) == '"' &&
    substr(value, nchar(value), nchar(value)) == '"'
  ) {
    value <- substr(
      value,
      2,
      nchar(value) - 1
    )
  }
  
  value
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


# Apply canonical shell ----------------------------------------------------

updated_categories <- character()

for (i in seq_len(nrow(categories))) {
  row <- categories[
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
  
  # Validate identity before modifying anything.
  title <- yaml_scalar(
    parts$yaml,
    "title"
  )
  
  category_id <- yaml_scalar(
    parts$yaml,
    "category-id"
  )
  
  page_type <- yaml_scalar(
    parts$yaml,
    "page-type"
  )
  
  if (
    is.na(title) ||
    title != row$page_title[[1]]
  ) {
    stop(
      "Support-category title mismatch: ",
      path
    )
  }
  
  if (
    is.na(category_id) ||
    category_id != row$category_id[[1]]
  ) {
    stop(
      "Support-category ID mismatch: ",
      path
    )
  }
  
  if (
    is.na(page_type) ||
    page_type != "support_category"
  ) {
    stop(
      "Invalid support-category page type: ",
      path
    )
  }
  
  yaml <- remove_yaml_value(
    parts$yaml,
    key = "subtitle"
  )
  
  yaml <- set_yaml_value(
    yaml,
    key = "toc",
    value = "false"
  )
  
  yaml <- set_body_width(
    yaml,
    width = "1000px"
  )
  
  yaml <- set_yaml_value(
    yaml,
    key = "body-classes",
    value = "support-category-page"
  )
  
  write_utf8(
    c(
      "---",
      yaml,
      "---",
      parts$body
    ),
    path
  )
  
  updated_categories <- c(
    updated_categories,
    path
  )
}


# Validation ---------------------------------------------------------------

for (path in updated_categories) {
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
        "^toc\\s*:\\s*false\\s*$",
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
        "^body-classes\\s*:\\s*support-category-page\\s*$",
        parts$yaml
      )
    )
  )
}

message(
  "Support-category shell applied successfully to ",
  length(updated_categories),
  " category pages."
)