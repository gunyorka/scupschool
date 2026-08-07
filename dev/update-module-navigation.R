manifest_path <- "dev/course-manifest.csv"

manifest <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

manifest <- manifest[
  order(manifest$module_number, manifest$page_order),
]

required_columns <- c(
  "module_number",
  "module_title",
  "page_order",
  "page_type",
  "page_title",
  "file_name",
  "relative_path"
)

missing_columns <- setdiff(required_columns, names(manifest))

if (length(missing_columns) > 0) {
  stop(
    "Missing manifest columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (anyDuplicated(manifest$relative_path)) {
  stop("The manifest contains duplicate page paths.")
}

missing_pages <- manifest$relative_path[
  !file.exists(manifest$relative_path)
]

if (length(missing_pages) > 0) {
  stop(
    "Manifest pages missing from the project:\n",
    paste(missing_pages, collapse = "\n")
  )
}


# Helpers -----------------------------------------------------------------

navigation_begin <- "<!-- BEGIN GENERATED MODULE NAVIGATION -->"
navigation_end   <- "<!-- END GENERATED MODULE NAVIGATION -->"

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

html_path <- function(file_name) {
  sub("\\.qmd$", ".html", file_name)
}

remove_existing_navigation <- function(lines, path) {
  begin <- which(lines == navigation_begin)
  end <- which(lines == navigation_end)
  
  if (length(begin) == 0 && length(end) == 0) {
    return(lines)
  }
  
  if (
    length(begin) != 1 ||
    length(end) != 1 ||
    begin >= end
  ) {
    stop(
      "Invalid generated-navigation markers in: ",
      path
    )
  }
  
  lines[-seq.int(begin, end)]
}

remove_legacy_module_link <- function(
    lines,
    current_row
) {
  legacy_link <- paste0(
    "[← ",
    current_row$module_number[[1]],
    ". modul — ",
    current_row$module_title[[1]],
    "](index.qmd)"
  )
  
  lines[
    trimws(lines) != legacy_link
  ]
}

insert_after_front_matter <- function(lines, navigation, path) {
  delimiters <- which(trimws(lines) == "---")
  
  if (
    length(delimiters) < 2 ||
    delimiters[[1]] != 1
  ) {
    stop("Valid YAML front matter not found in: ", path)
  }
  
  yaml_end <- delimiters[[2]]
  
  before <- lines[seq_len(yaml_end)]
  
  after <- if (yaml_end < length(lines)) {
    lines[(yaml_end + 1):length(lines)]
  } else {
    character()
  }
  
  # Remove blank lines immediately following the YAML.
  while (
    length(after) > 0 &&
    !nzchar(trimws(after[[1]]))
  ) {
    after <- after[-1]
  }
  
  c(
    before,
    "",
    navigation,
    "",
    after
  )
}

insert_before_module_pages <- function(
    lines,
    navigation,
    path
) {
  heading_position <- which(
    trimws(lines) == "## A modul oldalai"
  )
  
  if (length(heading_position) != 1) {
    stop(
      "Expected exactly one '## A modul oldalai' heading in: ",
      path
    )
  }
  
  heading_position <- heading_position[[1]]
  
  before <- if (heading_position > 1) {
    lines[seq_len(heading_position - 1)]
  } else {
    character()
  }
  
  after <- lines[
    heading_position:length(lines)
  ]
  
  while (
    length(before) > 0 &&
    !nzchar(trimws(before[[length(before)]]))
  ) {
    before <- before[-length(before)]
  }
  
  c(
    before,
    "",
    navigation,
    "",
    after
  )
}

navigation_link <- function(direction, row) {
  title <- escape_html(row$page_title[[1]])
  href <- html_path(row$file_name[[1]])
  
  if (direction == "previous") {
    return(
      paste0(
        '<a class="module-page-navigation__link ',
        'module-page-navigation__link--previous" ',
        'href="', href, '">',
        '<span class="module-page-navigation__label">Előző</span>',
        '<span class="module-page-navigation__title">',
        '← ', title,
        "</span>",
        "</a>"
      )
    )
  }
  
  if (direction == "next") {
    return(
      paste0(
        '<a class="module-page-navigation__link ',
        'module-page-navigation__link--next" ',
        'href="', href, '">',
        '<span class="module-page-navigation__label">Következő</span>',
        '<span class="module-page-navigation__title">',
        title, ' →',
        "</span>",
        "</a>"
      )
    )
  }
  
  stop("Unknown navigation direction: ", direction)
}

build_navigation <- function(
    current_row,
    current_position,
    total_pages,
    previous_row = NULL,
    next_row = NULL
) {
  links <- character()
  
  if (!is.null(previous_row)) {
    links <- c(
      links,
      navigation_link(
        "previous",
        previous_row
      )
    )
  }
  
  if (!is.null(next_row)) {
    links <- c(
      links,
      navigation_link(
        "next",
        next_row
      )
    )
  }
  
  is_overview <- identical(
    current_row$page_type[[1]],
    "overview"
  )
  
  utility_left <- if (is_overview) {
    paste0(
      '<span class="module-page-utility__spacer" ',
      'aria-hidden="true"></span>'
    )
  } else {
    paste0(
      '<a class="module-page-utility__overview" ',
      'href="index.html">',
      "← Moduláttekintő",
      "</a>"
    )
  }
  
  utility_row <- c(
    '<div class="module-page-utility">',
    utility_left,
    sprintf(
      paste0(
        '<span class="module-page-utility__position" ',
        'aria-label="Oldal helye a modulban">',
        "%d / %d",
        "</span>"
      ),
      current_position,
      total_pages
    ),
    "</div>"
  )
  
  navigation <- c(
    navigation_begin,
    utility_row,
    paste0(
      '<nav class="module-page-navigation" ',
      'aria-label="Modulon belüli navigáció">'
    ),
    links,
    "</nav>"
  )
  
  
  c(
    navigation,
    navigation_end
  )
}


# Update pages -------------------------------------------------------------

module_numbers <- sort(unique(manifest$module_number))

updated_pages <- character()

for (module_number in module_numbers) {
  module_rows <- manifest[
    manifest$module_number == module_number,
  ]
  
  module_rows <- module_rows[
    order(module_rows$page_order),
  ]
  
  for (i in seq_len(nrow(module_rows))) {
    current_row <- module_rows[i, , drop = FALSE]
    
    previous_row <- if (i > 1) {
      module_rows[i - 1, , drop = FALSE]
    } else {
      NULL
    }
    
    next_row <- if (i < nrow(module_rows)) {
      module_rows[i + 1, , drop = FALSE]
    } else {
      NULL
    }
    
    path <- current_row$relative_path[[1]]
    
    lines <- readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    lines <- remove_existing_navigation(
      lines,
      path
    )
    
    lines <- remove_legacy_module_link(
      lines = lines,
      current_row = current_row
    )
    
    navigation <- build_navigation(
      current_row = current_row,
      current_position = i,
      total_pages = nrow(module_rows),
      previous_row = previous_row,
      next_row = next_row
    )
    
    if (
      identical(
        current_row$page_type[[1]],
        "overview"
      )
    ) {
      lines <- insert_before_module_pages(
        lines = lines,
        navigation = navigation,
        path = path
      )
    } else {
      lines <- insert_after_front_matter(
        lines = lines,
        navigation = navigation,
        path = path
      )
    }
    
    writeLines(
      lines,
      con = path,
      useBytes = TRUE
    )
    
    updated_pages <- c(updated_pages, path)
  }
}


# Validation ---------------------------------------------------------------

marker_counts <- vapply(
  manifest$relative_path,
  function(path) {
    lines <- readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    c(
      begin = sum(lines == navigation_begin),
      end = sum(lines == navigation_end)
    )
  },
  numeric(2)
)

if (
  any(marker_counts["begin", ] != 1) ||
  any(marker_counts["end", ] != 1)
) {
  stop("Generated-navigation marker validation failed.")
}

milestone_paths <- manifest$relative_path[
  manifest$page_type == "milestone"
]

milestone_map_links <- vapply(
  milestone_paths,
  function(path) {
    lines <- readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    any(grepl(
      'href="../../index.html"',
      lines,
      fixed = TRUE
    ))
  },
  logical(1)
)

if (!all(milestone_map_links)) {
  stop("At least one milestone page is missing its milestone-map link.")
}

message(
  "Module navigation updated successfully on ",
  length(updated_pages),
  " pages."
)