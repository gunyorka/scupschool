# Support article sibling navigation ---------------------------------------

manifest_path <- "dev/support-manifest.csv"

navigation_begin <-
  "<!-- BEGIN GENERATED SUPPORT ARTICLE NAVIGATION -->"

navigation_end <-
  "<!-- END GENERATED SUPPORT ARTICLE NAVIGATION -->"


# Read and validate --------------------------------------------------------

support <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c(
  "category_id",
  "category_title",
  "item_type",
  "item_order",
  "page_id",
  "page_title",
  "file_name",
  "relative_path"
)

missing_columns <- setdiff(
  required_columns,
  names(support)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing support-manifest columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

articles <- support[
  support$item_type == "article",
  ,
  drop = FALSE
]

articles <- articles[
  order(
    articles$category_id,
    articles$item_order
  ),
  ,
  drop = FALSE
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

remove_generated_navigation <- function(
    lines,
    path
) {
  begin_position <- which(
    lines == navigation_begin
  )
  
  end_position <- which(
    lines == navigation_end
  )
  
  if (
    length(begin_position) == 0 &&
    length(end_position) == 0
  ) {
    return(lines)
  }
  
  if (
    length(begin_position) != 1 ||
    length(end_position) != 1 ||
    begin_position >= end_position
  ) {
    stop(
      "Invalid support-navigation markers in: ",
      path
    )
  }
  
  lines[
    -seq.int(
      begin_position,
      end_position
    )
  ]
}

remove_trailing_blank_lines <- function(lines) {
  while (
    length(lines) > 0 &&
    !nzchar(trimws(lines[[length(lines)]]))
  ) {
    lines <- lines[-length(lines)]
  }
  
  lines
}

build_navigation <- function(
    current_row,
    sibling_rows
) {
  links <- vapply(
    seq_len(nrow(sibling_rows)),
    function(i) {
      row <- sibling_rows[
        i,
        ,
        drop = FALSE
      ]
      
      paste0(
        "- [",
        row$page_title[[1]],
        "](",
        row$file_name[[1]],
        ")"
      )
    },
    character(1)
  )
  
  c(
    navigation_begin,
    "::: {.support-related-pages}",
    "",
    paste0(
      "**A [",
      current_row$category_title[[1]],
      "](index.qmd) további oldalai:**"
    ),
    "",
    links,
    "",
    ":::",
    navigation_end
  )
}


# Update article navigation ------------------------------------------------

updated_articles <- character()

for (i in seq_len(nrow(articles))) {
  current_row <- articles[
    i,
    ,
    drop = FALSE
  ]
  
  path <- current_row$relative_path[[1]]
  
  siblings <- articles[
    articles$category_id ==
      current_row$category_id[[1]] &
      articles$page_id !=
      current_row$page_id[[1]],
    ,
    drop = FALSE
  ]
  
  siblings <- siblings[
    order(siblings$item_order),
    ,
    drop = FALSE
  ]
  
  lines <- read_utf8(path)
  
  lines <- remove_generated_navigation(
    lines = lines,
    path = path
  )
  
  lines <- remove_trailing_blank_lines(
    lines
  )
  
  navigation <- build_navigation(
    current_row = current_row,
    sibling_rows = siblings
  )
  
  lines <- c(
    lines,
    "",
    navigation,
    ""
  )
  
  write_utf8(
    lines,
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
  
  if (
    sum(lines == navigation_begin) != 1 ||
    sum(lines == navigation_end) != 1
  ) {
    stop(
      "Support-navigation marker validation failed: ",
      path
    )
  }
}

message(
  "Support navigation updated successfully on ",
  length(updated_articles),
  " articles."
)