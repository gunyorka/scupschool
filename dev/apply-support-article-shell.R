manifest_path <- "dev/support-manifest.csv"

utility_begin <-
  "<!-- BEGIN GENERATED SUPPORT ARTICLE UTILITY -->"

utility_end <-
  "<!-- END GENERATED SUPPORT ARTICLE UTILITY -->"


# Read and validate --------------------------------------------------------

manifest <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

articles <- manifest[
  manifest$item_type == "article",
  ,
  drop = FALSE
]

articles <- articles[
  order(
    articles$category_order,
    articles$item_order
  ),
]

if (nrow(articles) == 0) {
  stop("No support articles found in the manifest.")
}

if (anyDuplicated(articles$page_id)) {
  stop("Duplicate support article IDs found.")
}

if (anyDuplicated(articles$relative_path)) {
  stop("Duplicate support article paths found.")
}

missing_articles <- articles$relative_path[
  !file.exists(articles$relative_path)
]

if (length(missing_articles) > 0) {
  stop(
    "Missing support articles:\n",
    paste(missing_articles, collapse = "\n")
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

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
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

remove_leading_blank_lines <- function(lines) {
  while (
    length(lines) > 0 &&
    !nzchar(trimws(lines[[1]]))
  ) {
    lines <- lines[-1]
  }
  
  lines
}


# Generated category breadcrumb -------------------------------------------

support_utility <- function(category_title) {
  c(
    utility_begin,
    paste0(
      '<div class="support-article-utility">',
      '<a class="support-article-utility__category" ',
      'href="index.html">',
      "← ",
      escape_html(category_title),
      "</a>",
      "</div>"
    ),
    utility_end
  )
}

remove_existing_utility <- function(
    body,
    path
) {
  begin_position <- which(
    body == utility_begin
  )
  
  end_position <- which(
    body == utility_end
  )
  
  if (
    length(begin_position) == 0 &&
    length(end_position) == 0
  ) {
    return(body)
  }
  
  if (
    length(begin_position) != 1 ||
    length(end_position) != 1 ||
    begin_position >= end_position
  ) {
    stop(
      "Invalid support utility markers in: ",
      path
    )
  }
  
  body[
    -seq.int(
      begin_position,
      end_position
    )
  ]
}

remove_legacy_links <- function(
    body,
    category_title
) {
  legacy_category_link <- paste0(
    "[← ",
    category_title,
    "](index.qmd)"
  )
  
  legacy_home_links <- c(
    "[Segítségtár kezdőlap](../index.qmd)",
    "[← Vissza a Segítségtárhoz](../index.qmd)"
  )
  
  body[
    !trimws(body) %in%
      c(
        legacy_category_link,
        legacy_home_links
      )
  ]
}

set_support_utility <- function(
    body,
    category_title,
    path
) {
  body <- remove_existing_utility(
    body,
    path
  )
  
  body <- remove_legacy_links(
    body,
    category_title
  )
  
  body <- remove_leading_blank_lines(body)
  
  c(
    support_utility(category_title),
    "",
    body
  )
}


# Apply shell --------------------------------------------------------------

updated_articles <- character()

for (i in seq_len(nrow(articles))) {
  row <- articles[
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
    value = "support-content-page"
  )
  
  body <- set_support_utility(
    body = parts$body,
    category_title =
      row$category_title[[1]],
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
  
  updated_articles <- c(
    updated_articles,
    path
  )
}


# Validation ---------------------------------------------------------------

for (path in updated_articles) {
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
        paste0(
          "^body-classes\\s*:\\s*",
          "support-content-page\\s*$"
        ),
        parts$yaml
      )
    ),
    
    sum(parts$body == utility_begin) == 1,
    sum(parts$body == utility_end) == 1,
    
    !any(
      grepl(
        "Segítségtár kezdőlap",
        parts$body,
        fixed = TRUE
      )
    ),
    
    !any(
      grepl(
        "BEGIN GENERATED MODULE NAVIGATION",
        parts$body,
        fixed = TRUE
      )
    )
  )
}

message(
  "Support-article shell applied successfully to ",
  length(updated_articles),
  " articles."
)