# Scup School project audit ------------------------------------------------
#
# Read-only validation.
# Does not modify project files.
#
# Usage:
#
# source("dev/audit-project.R", encoding = "UTF-8")
# audit_project()
#
# After a Quarto render:
#
# audit_project(check_rendered = TRUE)


audit_project <- function(
    check_rendered = FALSE,
    stop_on_error = TRUE
) {
  
  # -----------------------------------------------------------------------
  # Setup
  # -----------------------------------------------------------------------
  
  required_project_files <- c(
    "_quarto.yml",
    "dev/course-manifest.csv",
    "dev/support-manifest.csv"
  )
  
  missing_project_files <- required_project_files[
    !file.exists(required_project_files)
  ]
  
  if (length(missing_project_files) > 0) {
    stop(
      "Required project files are missing:\n",
      paste(missing_project_files, collapse = "\n")
    )
  }
  
  course <- read.csv(
    "dev/course-manifest.csv",
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  support <- read.csv(
    "dev/support-manifest.csv",
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  errors <- character()
  warnings <- character()
  info <- character()
  
  
  add_error <- function(...) {
    errors <<- c(
      errors,
      paste0(...)
    )
  }
  
  add_warning <- function(...) {
    warnings <<- c(
      warnings,
      paste0(...)
    )
  }
  
  add_info <- function(...) {
    info <<- c(
      info,
      paste0(...)
    )
  }
  
  
  # -----------------------------------------------------------------------
  # Helpers
  # -----------------------------------------------------------------------
  
  read_utf8 <- function(path) {
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    )
  }
  
  
  split_front_matter <- function(
    lines,
    path
  ) {
    delimiters <- which(
      trimws(lines) == "---"
    )
    
    if (
      length(delimiters) < 2 ||
      delimiters[[1]] != 1
    ) {
      return(NULL)
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
    
    if (length(hits) == 0) {
      return(NA_character_)
    }
    
    if (length(hits) > 1) {
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
  
  
  has_yaml_line <- function(
    yaml,
    pattern
  ) {
    any(
      grepl(
        pattern,
        yaml
      )
    )
  }
  
  
  count_exact <- function(
    lines,
    value
  ) {
    sum(lines == value)
  }
  
  
  source_target_exists <- function(
    source_path,
    target
  ) {
    
    target <- sub(
      "#.*$",
      "",
      target
    )
    
    target <- trimws(target)
    
    if (!nzchar(target)) {
      return(TRUE)
    }
    
    # External / special links
    if (
      grepl(
        "^(https?://|mailto:|tel:|javascript:)",
        target,
        ignore.case = TRUE
      )
    ) {
      return(TRUE)
    }
    
    # Pure anchor
    if (
      startsWith(
        target,
        "#"
      )
    ) {
      return(TRUE)
    }
    
    # Remove query string
    target <- sub(
      "\\?.*$",
      "",
      target
    )
    
    source_dir <- dirname(
      source_path
    )
    
    resolved <- file.path(
      source_dir,
      target
    )
    
    resolved <- normalizePath(
      resolved,
      winslash = "/",
      mustWork = FALSE
    )
    
    # Raw HTML links generally use .html.
    # Validate against the corresponding QMD source.
    if (
      grepl(
        "\\.html$",
        resolved,
        ignore.case = TRUE
      )
    ) {
      qmd_target <- sub(
        "\\.html$",
        ".qmd",
        resolved,
        ignore.case = TRUE
      )
      
      return(
        file.exists(qmd_target)
      )
    }
    
    file.exists(resolved) ||
      dir.exists(resolved)
  }
  
  
  extract_internal_links <- function(
    lines
  ) {
    
    text <- paste(
      lines,
      collapse = "\n"
    )
    
    targets <- character()
    
    
    # Markdown links: [text](target)
    markdown_matches <- gregexpr(
      "\\]\\(([^)]+)\\)",
      text,
      perl = TRUE
    )
    
    markdown_hits <- regmatches(
      text,
      markdown_matches
    )[[1]]
    
    if (
      length(markdown_hits) > 0 &&
      markdown_hits[[1]] != "-1"
    ) {
      markdown_targets <- sub(
        "^\\]\\(",
        "",
        markdown_hits
      )
      
      markdown_targets <- sub(
        "\\)$",
        "",
        markdown_targets
      )
      
      # Ignore optional Markdown link titles
      markdown_targets <- sub(
        "\\s+[\"'].*$",
        "",
        markdown_targets
      )
      
      targets <- c(
        targets,
        markdown_targets
      )
    }
    
    
    # Raw HTML href="..."
    html_matches <- gregexpr(
      'href="([^"]+)"',
      text,
      perl = TRUE
    )
    
    html_hits <- regmatches(
      text,
      html_matches
    )[[1]]
    
    if (
      length(html_hits) > 0 &&
      html_hits[[1]] != "-1"
    ) {
      html_targets <- sub(
        '^href="',
        "",
        html_hits
      )
      
      html_targets <- sub(
        '"$',
        "",
        html_targets
      )
      
      targets <- c(
        targets,
        html_targets
      )
    }
    
    unique(targets)
  }
  
  
  # -----------------------------------------------------------------------
  # 1. Manifest integrity
  # -----------------------------------------------------------------------
  
  expected_course_types <- c(
    "overview",
    "core",
    "readiness",
    "meeting_pack",
    "milestone"
  )
  
  if (
    anyDuplicated(
      course$page_id
    )
  ) {
    add_error(
      "Duplicate page_id values in course manifest."
    )
  }
  
  if (
    anyDuplicated(
      course$relative_path
    )
  ) {
    add_error(
      "Duplicate relative_path values in course manifest."
    )
  }
  
  unknown_course_types <- setdiff(
    unique(course$page_type),
    expected_course_types
  )
  
  if (
    length(unknown_course_types) > 0
  ) {
    add_error(
      "Unknown course page types: ",
      paste(
        unknown_course_types,
        collapse = ", "
      )
    )
  }
  
  
  if (
    anyDuplicated(
      support$page_id
    )
  ) {
    add_error(
      "Duplicate page_id values in support manifest."
    )
  }
  
  if (
    anyDuplicated(
      support$relative_path
    )
  ) {
    add_error(
      "Duplicate relative_path values in support manifest."
    )
  }
  
  
  if (
    nrow(course) != 83
  ) {
    add_warning(
      "Course manifest currently contains ",
      nrow(course),
      " pages; canonical baseline was 83."
    )
  }
  
  if (
    nrow(support) != 24
  ) {
    add_warning(
      "Support manifest currently contains ",
      nrow(support),
      " pages; canonical baseline was 24."
    )
  }
  
  
  # -----------------------------------------------------------------------
  # 2. Manifest vs filesystem
  # -----------------------------------------------------------------------
  
  missing_course <- course$relative_path[
    !file.exists(course$relative_path)
  ]
  
  if (
    length(missing_course) > 0
  ) {
    add_error(
      "Course manifest pages missing:\n  ",
      paste(
        missing_course,
        collapse = "\n  "
      )
    )
  }
  
  
  missing_support <- support$relative_path[
    !file.exists(support$relative_path)
  ]
  
  if (
    length(missing_support) > 0
  ) {
    add_error(
      "Support manifest pages missing:\n  ",
      paste(
        missing_support,
        collapse = "\n  "
      )
    )
  }
  
  
  actual_module_qmd <- list.files(
    "modules",
    pattern = "\\.qmd$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  actual_module_qmd <- gsub(
    "\\\\",
    "/",
    actual_module_qmd
  )
  
  
  extra_module_pages <- setdiff(
    actual_module_qmd,
    course$relative_path
  )
  
  if (
    length(extra_module_pages) > 0
  ) {
    add_warning(
      "Module QMD files not represented in the manifest:\n  ",
      paste(
        extra_module_pages,
        collapse = "\n  "
      )
    )
  }
  
  
  actual_support_qmd <- list.files(
    "support",
    pattern = "\\.qmd$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  actual_support_qmd <- gsub(
    "\\\\",
    "/",
    actual_support_qmd
  )
  
  
  expected_support_qmd <- c(
    "support/index.qmd",
    support$relative_path
  )
  
  
  extra_support_pages <- setdiff(
    actual_support_qmd,
    expected_support_qmd
  )
  
  if (
    length(extra_support_pages) > 0
  ) {
    add_warning(
      "Support QMD files not represented in the support architecture:\n  ",
      paste(
        extra_support_pages,
        collapse = "\n  "
      )
    )
  }
  
  
  # -----------------------------------------------------------------------
  # 3. Course-page metadata and generated structures
  # -----------------------------------------------------------------------
  
  nav_begin <-
    "<!-- BEGIN GENERATED MODULE NAVIGATION -->"
  
  nav_end <-
    "<!-- END GENERATED MODULE NAVIGATION -->"
  
  route_begin <-
    "<!-- BEGIN GENERATED MODULE ROUTE -->"
  
  route_end <-
    "<!-- END GENERATED MODULE ROUTE -->"
  
  preparation_begin <-
    "<!-- BEGIN GENERATED MEETING PREPARATION CONTENT -->"
  
  preparation_end <-
    "<!-- END GENERATED MEETING PREPARATION CONTENT -->"
  
  meeting_support_begin <-
    "<!-- BEGIN GENERATED MEETING SUPPORT -->"
  
  meeting_support_end <-
    "<!-- END GENERATED MEETING SUPPORT -->"
  
  milestone_link_begin <-
    "<!-- BEGIN GENERATED MILESTONE NEXT LINK -->"
  
  milestone_link_end <-
    "<!-- END GENERATED MILESTONE NEXT LINK -->"
  
  
  for (
    i in seq_len(
      nrow(course)
    )
  ) {
    
    row <- course[
      i,
      ,
      drop = FALSE
    ]
    
    path <- row$relative_path[[1]]
    
    if (!file.exists(path)) {
      next
    }
    
    lines <- read_utf8(path)
    
    parts <- split_front_matter(
      lines,
      path
    )
    
    if (is.null(parts)) {
      add_error(
        "Invalid YAML front matter: ",
        path
      )
      
      next
    }
    
    
    title <- yaml_scalar(
      parts$yaml,
      "title"
    )
    
    if (
      is.na(title) ||
      title != row$page_title[[1]]
    ) {
      add_error(
        "Title mismatch: ",
        path,
        "\n  manifest: ",
        row$page_title[[1]],
        "\n  page: ",
        ifelse(
          is.na(title),
          "<missing>",
          title
        )
      )
    }
    
    
    page_id <- yaml_scalar(
      parts$yaml,
      "page-id"
    )
    
    if (
      is.na(page_id) ||
      page_id != row$page_id[[1]]
    ) {
      add_error(
        "page-id mismatch: ",
        path
      )
    }
    
    
    page_type <- yaml_scalar(
      parts$yaml,
      "page-type"
    )
    
    if (
      is.na(page_type) ||
      page_type != row$page_type[[1]]
    ) {
      add_error(
        "page-type mismatch: ",
        path
      )
    }
    
    
    module_number <- yaml_scalar(
      parts$yaml,
      "module-number"
    )
    
    if (
      is.na(module_number) ||
      as.character(
        row$module_number[[1]]
      ) != module_number
    ) {
      add_error(
        "module-number mismatch: ",
        path
      )
    }
    
    
    page_order <- yaml_scalar(
      parts$yaml,
      "page-order"
    )
    
    if (
      is.na(page_order) ||
      as.character(
        row$page_order[[1]]
      ) != page_order
    ) {
      add_error(
        "page-order mismatch: ",
        path
      )
    }
    
    
    # All module pages should have generated navigation
    if (
      count_exact(
        parts$body,
        nav_begin
      ) != 1 ||
      count_exact(
        parts$body,
        nav_end
      ) != 1
    ) {
      add_error(
        "Generated module navigation marker problem: ",
        path
      )
    }
    
    
    # Overview-specific route
    if (
      row$page_type[[1]] ==
      "overview"
    ) {
      if (
        count_exact(
          parts$body,
          route_begin
        ) != 1 ||
        count_exact(
          parts$body,
          route_end
        ) != 1
      ) {
        add_error(
          "Generated module route marker problem: ",
          path
        )
      }
    }
    
    
    # Meeting preparation
    if (
      row$page_type[[1]] ==
      "readiness"
    ) {
      if (
        count_exact(
          parts$body,
          preparation_begin
        ) != 1 ||
        count_exact(
          parts$body,
          preparation_end
        ) != 1
      ) {
        add_error(
          "Meeting-preparation marker problem: ",
          path
        )
      }
    }
    
    
    # Meeting pack
    if (
      row$page_type[[1]] ==
      "meeting_pack"
    ) {
      if (
        count_exact(
          parts$body,
          meeting_support_begin
        ) != 1 ||
        count_exact(
          parts$body,
          meeting_support_end
        ) != 1
      ) {
        add_error(
          "Meeting-support marker problem: ",
          path
        )
      }
    }
    
    
    # Milestone
    if (
      row$page_type[[1]] ==
      "milestone"
    ) {
      if (
        count_exact(
          parts$body,
          milestone_link_begin
        ) != 1 ||
        count_exact(
          parts$body,
          milestone_link_end
        ) != 1
      ) {
        add_error(
          "Milestone transition marker problem: ",
          path
        )
      }
    }
    
    
    # Approved 1000px page types
    if (
      row$page_type[[1]] %in%
      c(
        "overview",
        "core",
        "readiness",
        "meeting_pack",
        "milestone"
      )
    ) {
      if (
        !has_yaml_line(
          parts$yaml,
          "^\\s+body-width\\s*:\\s*1000px\\s*$"
        )
      ) {
        add_error(
          "Missing body-width: 1000px: ",
          path
        )
      }
    }
  }
  
  
  # -----------------------------------------------------------------------
  # 4. Support-page metadata
  # -----------------------------------------------------------------------
  
  support_utility_begin <-
    "<!-- BEGIN GENERATED SUPPORT ARTICLE UTILITY -->"
  
  support_utility_end <-
    "<!-- END GENERATED SUPPORT ARTICLE UTILITY -->"
  
  support_article_list_begin <-
    "<!-- BEGIN GENERATED SUPPORT ARTICLE LIST -->"
  
  support_article_list_end <-
    "<!-- END GENERATED SUPPORT ARTICLE LIST -->"
  
  
  for (
    i in seq_len(
      nrow(support)
    )
  ) {
    
    row <- support[
      i,
      ,
      drop = FALSE
    ]
    
    path <- row$relative_path[[1]]
    
    if (!file.exists(path)) {
      next
    }
    
    lines <- read_utf8(path)
    
    parts <- split_front_matter(
      lines,
      path
    )
    
    if (is.null(parts)) {
      add_error(
        "Invalid support YAML front matter: ",
        path
      )
      
      next
    }
    
    
    if (
      row$item_type[[1]] ==
      "article"
    ) {
      
      title <- yaml_scalar(
        parts$yaml,
        "title"
      )
      
      if (
        is.na(title) ||
        title != row$page_title[[1]]
      ) {
        add_error(
          "Support article title mismatch: ",
          path
        )
      }
      
      
      if (
        count_exact(
          parts$body,
          support_utility_begin
        ) != 1 ||
        count_exact(
          parts$body,
          support_utility_end
        ) != 1
      ) {
        add_error(
          "Support article utility marker problem: ",
          path
        )
      }
      
      
      if (
        !has_yaml_line(
          parts$yaml,
          "^\\s+body-width\\s*:\\s*1000px\\s*$"
        )
      ) {
        add_error(
          "Support article missing body-width: 1000px: ",
          path
        )
      }
    }
    
    
    if (
      row$item_type[[1]] ==
      "category"
    ) {
      if (
        count_exact(
          parts$body,
          support_article_list_begin
        ) != 1 ||
        count_exact(
          parts$body,
          support_article_list_end
        ) != 1
      ) {
        add_error(
          "Support category article-list marker problem: ",
          path
        )
      }
    }
  }
  
  
  # -----------------------------------------------------------------------
  # 5. Internal link audit
  # -----------------------------------------------------------------------
  
  qmd_files <- c(
    "index.qmd",
    list.files(
      "course",
      pattern = "\\.qmd$",
      recursive = TRUE,
      full.names = TRUE
    ),
    list.files(
      "modules",
      pattern = "\\.qmd$",
      recursive = TRUE,
      full.names = TRUE
    ),
    list.files(
      "support",
      pattern = "\\.qmd$",
      recursive = TRUE,
      full.names = TRUE
    )
  )
  
  qmd_files <- unique(
    gsub(
      "\\\\",
      "/",
      qmd_files
    )
  )
  
  
  broken_links <- data.frame(
    source = character(),
    target = character(),
    stringsAsFactors = FALSE
  )
  
  
  for (path in qmd_files) {
    
    if (!file.exists(path)) {
      next
    }
    
    targets <- extract_internal_links(
      read_utf8(path)
    )
    
    if (length(targets) == 0) {
      next
    }
    
    for (target in targets) {
      
      if (
        !source_target_exists(
          source_path = path,
          target = target
        )
      ) {
        broken_links <- rbind(
          broken_links,
          data.frame(
            source = path,
            target = target,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  
  if (
    nrow(broken_links) > 0
  ) {
    add_error(
      "Broken internal links found:\n",
      paste(
        paste0(
          "  ",
          broken_links$source,
          " -> ",
          broken_links$target
        ),
        collapse = "\n"
      )
    )
  }
  
  
  # -----------------------------------------------------------------------
  # 6. Placeholder audit
  # -----------------------------------------------------------------------
  
  placeholder_patterns <- c(
    "navigációs prototípus",
    "[Rövid bevezető helye.]",
    "[A végleges tananyag helye.]",
    "[Az opcionális tananyag helye.]",
    "modulspecifikus értékelési szempont helye",
    "modulspecifikus meetingtipp helye",
    "Szükséges vagy hasznos eszköz helye",
    "Az első ajánlott cél helye",
    "Az első, modulhoz kapcsolódó gyakorlati tanács helye"
  )
  
  
  placeholder_pages <- data.frame(
    path = character(),
    hits = integer(),
    stringsAsFactors = FALSE
  )
  
  
  authored_paths <- c(
    course$relative_path,
    support$relative_path
  )
  
  
  for (path in authored_paths) {
    
    if (!file.exists(path)) {
      next
    }
    
    lines <- read_utf8(path)
    
    hit_count <- sum(
      vapply(
        placeholder_patterns,
        function(pattern) {
          any(
            grepl(
              pattern,
              lines,
              fixed = TRUE
            )
          )
        },
        logical(1)
      )
    )
    
    if (hit_count > 0) {
      placeholder_pages <- rbind(
        placeholder_pages,
        data.frame(
          path = path,
          hits = hit_count,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  
  add_info(
    "Pages containing known placeholder content: ",
    nrow(placeholder_pages)
  )
  
  
  # -----------------------------------------------------------------------
  # 7. Rendered-site audit
  # -----------------------------------------------------------------------
  
  if (check_rendered) {
    
    if (!dir.exists("_site")) {
      add_error(
        "check_rendered = TRUE but _site/ does not exist."
      )
    } else {
      
      expected_rendered <- c(
        "index.html",
        
        sub(
          "\\.qmd$",
          ".html",
          course$relative_path
        ),
        
        "support/index.html",
        
        sub(
          "\\.qmd$",
          ".html",
          support$relative_path
        ),
        
        sub(
          "\\.qmd$",
          ".html",
          list.files(
            "course",
            pattern = "\\.qmd$",
            recursive = TRUE,
            full.names = TRUE
          )
        )
      )
      
      expected_rendered <- file.path(
        "_site",
        expected_rendered
      )
      
      missing_rendered <- expected_rendered[
        !file.exists(expected_rendered)
      ]
      
      if (
        length(missing_rendered) > 0
      ) {
        add_error(
          "Rendered HTML files missing:\n  ",
          paste(
            missing_rendered,
            collapse = "\n  "
          )
        )
      }
    }
  }
  
  
  # -----------------------------------------------------------------------
  # Report
  # -----------------------------------------------------------------------
  
  cat(
    "\n",
    "Scup School project audit\n",
    "=========================\n\n",
    sep = ""
  )
  
  
  cat(
    "Course pages:   ",
    nrow(course),
    "\n",
    sep = ""
  )
  
  cat(
    "Support pages:  ",
    nrow(support),
    " + support landing page\n",
    sep = ""
  )
  
  cat(
    "Placeholders:   ",
    nrow(placeholder_pages),
    " pages\n",
    sep = ""
  )
  
  cat(
    "Errors:         ",
    length(errors),
    "\n",
    sep = ""
  )
  
  cat(
    "Warnings:       ",
    length(warnings),
    "\n\n",
    sep = ""
  )
  
  
  if (length(info) > 0) {
    cat(
      "INFO\n----\n"
    )
    
    cat(
      paste0(
        "• ",
        info,
        collapse = "\n"
      ),
      "\n\n"
    )
  }
  
  
  if (length(warnings) > 0) {
    cat(
      "WARNINGS\n--------\n"
    )
    
    cat(
      paste0(
        "• ",
        warnings,
        collapse = "\n"
      ),
      "\n\n"
    )
  }
  
  
  if (length(errors) > 0) {
    cat(
      "ERRORS\n------\n"
    )
    
    cat(
      paste0(
        "• ",
        errors,
        collapse = "\n\n"
      ),
      "\n\n"
    )
  }
  
  
  if (
    length(errors) == 0
  ) {
    cat(
      "✓ Structural audit passed.\n"
    )
  }
  
  
  result <- list(
    passed = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    placeholder_pages = placeholder_pages,
    broken_links = broken_links
  )
  
  
  if (
    stop_on_error &&
    length(errors) > 0
  ) {
    stop(
      "Project audit failed with ",
      length(errors),
      " error(s).",
      call. = FALSE
    )
  }
  
  
  invisible(result)
}