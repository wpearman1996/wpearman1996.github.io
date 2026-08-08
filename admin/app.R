# Content editor for the personal website/CV.
#
# Run with shiny::runApp("admin") from the project root, or open this
# file in RStudio and click "Run App" -- either way Shiny sets the
# working directory to this file's folder, so PROJECT_ROOT below
# resolves to the site root regardless of where the app is launched
# from.
library(shiny)
library(DT)
library(readr)
library(dplyr)
library(yaml)

PROJECT_ROOT <- normalizePath("..")
CV_ENTRIES_PATH <- file.path(PROJECT_ROOT, "content", "cv_entries.csv")
PUBS_PATH <- file.path(PROJECT_ROOT, "content", "pubs.csv")
META_PATH <- file.path(PROJECT_ROOT, "content", "cv_meta.yml")
RECIPES_PATH <- file.path(PROJECT_ROOT, "content", "recipes.yml")
# Gitignored -- ingredients/instructions for protected recipes. Never
# committed; only ever read/written locally. See recipes.qmd.
PROTECTED_RECIPES_PATH <- file.path(PROJECT_ROOT, "content", "recipes_protected.yml")
PROJECTS_PATH <- file.path(PROJECT_ROOT, "content", "projects.yml")
PRINTS_PATH <- file.path(PROJECT_ROOT, "content", "prints.yml")
PRINTS_UPLOADS_DIR <- file.path(PROJECT_ROOT, "prints_uploads")

# Shiny's default 5MB upload cap is fine for photos but too small for
# 3D model files (STL/3MF exports routinely run tens of MB) -- this app
# only ever runs locally against the user's own machine, so there's no
# shared-server resource risk in raising it.
options(shiny.maxRequestSize = 300 * 1024^2)

CV_ENTRIES_COLS <- c("section", "year", "sort_year", "title", "where", "location",
                      "show_full_cv", "show_onepage_cv", "show_website")
PUBS_COLS <- c("index", "year", "authors", "title", "journal", "volume", "pages", "doi",
               "show_full_cv", "show_onepage_cv", "show_website")

SECTION_CHOICES <- c(
  "Education" = "education", "Position" = "position", "Award" = "award",
  "Grant" = "grant", "Service" = "service", "Professional development" = "prof-dev",
  "Teaching" = "teaching", "Talk" = "talk"
)
SECTIONS_WITH_WHERE <- c("education", "position")

# ---- shared helpers ---------------------------------------------------

read_cv_entries <- function() {
  d <- readr::read_csv(CV_ENTRIES_PATH, show_col_types = FALSE)
  d$.row_id <- seq_len(nrow(d))
  d
}

write_cv_entries <- function(d) {
  d <- d[, CV_ENTRIES_COLS]
  readr::write_csv(d, CV_ENTRIES_PATH, na = "")
}

read_pubs <- function() {
  d <- readr::read_csv(PUBS_PATH, show_col_types = FALSE)
  d$.row_id <- seq_len(nrow(d))
  d
}

write_pubs <- function(d) {
  d <- d[, PUBS_COLS]
  readr::write_csv(d, PUBS_PATH, na = "")
}

blank_to_na <- function(x) if (is.null(x) || !nzchar(trimws(x))) NA_character_ else x

# Avoids silently clobbering an existing file when two uploads share a
# name (e.g. two photos both called "IMG_0001.jpg" from different phones).
make_unique_filename <- function(dir, name) {
  base <- tools::file_path_sans_ext(name)
  ext <- tools::file_ext(name)
  candidate <- name
  i <- 1
  while (file.exists(file.path(dir, candidate))) {
    i <- i + 1
    candidate <- paste0(base, "_", i, if (nzchar(ext)) paste0(".", ext) else "")
  }
  candidate
}

# ---- UI -----------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Website content editor"),
  tabsetPanel(

    tabPanel("CV entries",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput("cv_section", "Section", choices = SECTION_CHOICES),
          actionButton("cv_new", "Add new entry", class = "btn-primary"),
          hr(),
          h4("Entry details"),
          conditionalPanel("input.cv_section == 'education' || input.cv_section == 'position'",
            textInput("cv_where", "Where (institution/organisation)"),
            textInput("cv_location", "Location")
          ),
          conditionalPanel("input.cv_section != 'teaching'",
            textInput("cv_year", "Year (display text, e.g. \"2024–present\")"),
            numericInput("cv_sort_year", "Sort year (numeric, controls ordering)", value = NA)
          ),
          textAreaInput("cv_title", "Title / description", rows = 3),
          checkboxInput("cv_show_full_cv", "Show on full CV", value = TRUE),
          checkboxInput("cv_show_onepage_cv", "Show on 1-page CV", value = FALSE),
          checkboxInput("cv_show_website", "Show on website", value = TRUE),
          actionButton("cv_save", "Save entry", class = "btn-success"),
          actionButton("cv_delete", "Delete entry", class = "btn-danger"),
          width = 4
        ),
        mainPanel(
          p("Click a row to edit it."),
          DTOutput("cv_table"),
          width = 8
        )
      )
    ),

    tabPanel("Publications",
      br(),
      sidebarLayout(
        sidebarPanel(
          actionButton("pub_new", "Add new publication", class = "btn-primary"),
          hr(),
          h4("Publication details"),
          helpText("Index is assigned automatically and can't be edited."),
          verbatimTextOutput("pub_index_display"),
          textInput("pub_year", "Year"),
          textAreaInput("pub_authors", "Authors (e.g. \"Smith, J. & Pearman, W.S.\")", rows = 2),
          textAreaInput("pub_title", "Title", rows = 2),
          textInput("pub_journal", "Journal"),
          textInput("pub_volume", "Volume"),
          textInput("pub_pages", "Pages"),
          textInput("pub_doi", "DOI (no https:// prefix, e.g. \"10.1111/jpy.70140\")"),
          checkboxInput("pub_show_full_cv", "Show on full CV", value = TRUE),
          checkboxInput("pub_show_onepage_cv", "Show on 1-page CV", value = FALSE),
          checkboxInput("pub_show_website", "Show on website", value = TRUE),
          actionButton("pub_save", "Save publication", class = "btn-success"),
          actionButton("pub_delete", "Delete publication", class = "btn-danger"),
          width = 4
        ),
        mainPanel(
          p("Click a row to edit it."),
          DTOutput("pub_table"),
          width = 8
        )
      )
    ),

    tabPanel("Profile text",
      br(),
      fluidRow(
        column(6,
          textInput("meta_name", "Name"),
          textInput("meta_title", "Title (e.g. \"Dr\")"),
          textInput("meta_position", "Position"),
          textInput("meta_employer", "Employer"),
          textInput("meta_email", "Email"),
          textInput("meta_website", "Website URL"),
          textInput("meta_orcid", "ORCID"),
          textInput("meta_orcid_url", "ORCID URL")
        ),
        column(6,
          textInput("meta_scholar_id", "Google Scholar ID"),
          textInput("meta_scholar_url", "Google Scholar URL"),
          textInput("meta_github", "GitHub URL"),
          textInput("meta_bluesky", "Bluesky URL"),
          textInput("meta_linkedin", "LinkedIn URL")
        )
      ),
      textAreaInput("meta_speciality", "Research summary", rows = 8, width = "100%"),
      textAreaInput("meta_peer_review", "Peer review activities (one journal per line)",
                     rows = 4, width = "100%"),
      actionButton("meta_save", "Save profile text", class = "btn-success")
    ),

    tabPanel("Recipes",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput("recipe_select", "Recipe", choices = NULL),
          helpText("Password-protected recipes aren't editable here -- use the",
                    "\"Locked recipes\" tab for those instead."),
          actionButton("recipe_new", "Add new recipe", class = "btn-primary"),
          hr(),
          textInput("recipe_name", "Name"),
          textInput("recipe_slug", "Slug (unique, no spaces -- auto-filled from name)"),
          numericInput("recipe_servings", "Base servings", value = 4, min = 1),
          textInput("recipe_source_name", "Source name (optional, e.g. \"Otago Daily Times\")"),
          textInput("recipe_source_url", "Source URL (optional)"),
          textAreaInput("recipe_instructions", "Instructions", rows = 4),
          h4("Ingredients"),
          uiOutput("ingredient_rows"),
          actionButton("ingredient_add", "+ Add ingredient"),
          hr(),
          actionButton("recipe_save", "Save recipe", class = "btn-success"),
          actionButton("recipe_delete", "Delete recipe", class = "btn-danger"),
          width = 5
        ),
        mainPanel(
          h4("Preview"),
          tableOutput("recipe_preview"),
          width = 7
        )
      )
    ),

    tabPanel("Locked recipes",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput("locked_recipe_select", "Locked recipe", choices = NULL),
          helpText("Ingredients/instructions saved here go to the gitignored",
                    "content/recipes_protected.yml, never committed to git --",
                    "everything else (name, source, servings) is public",
                    "metadata that stays in the tracked content/recipes.yml,",
                    "same as public recipes."),
          actionButton("locked_recipe_new", "Add new locked recipe", class = "btn-primary"),
          hr(),
          textInput("locked_recipe_name", "Name"),
          textInput("locked_recipe_slug", "Slug (unique, no spaces -- auto-filled from name)"),
          numericInput("locked_recipe_servings", "Base servings", value = 4, min = 1),
          textInput("locked_recipe_source_name", "Source name (e.g. \"Otago Daily Times\")"),
          textInput("locked_recipe_source_url", "Source URL"),
          textAreaInput("locked_recipe_instructions", "Instructions", rows = 4),
          h4("Ingredients"),
          uiOutput("locked_ingredient_rows"),
          actionButton("locked_ingredient_add", "+ Add ingredient"),
          hr(),
          actionButton("locked_recipe_save", "Save locked recipe", class = "btn-success"),
          actionButton("locked_recipe_delete", "Delete locked recipe", class = "btn-danger"),
          width = 5
        ),
        mainPanel(
          h4("Preview"),
          tableOutput("locked_recipe_preview"),
          width = 7
        )
      )
    ),

    tabPanel("Projects",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput("project_select", "Project", choices = NULL),
          actionButton("project_new", "Add new project", class = "btn-primary"),
          hr(),
          textInput("project_title", "Title"),
          textAreaInput("project_body", "Body (markdown -- can include links, italics, etc.)",
                         rows = 12, width = "100%"),
          actionButton("project_save", "Save project", class = "btn-success"),
          actionButton("project_delete", "Delete project", class = "btn-danger"),
          width = 5
        ),
        mainPanel(width = 7)
      )
    ),

    tabPanel("3D Prints",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput("print_select", "Print", choices = NULL),
          actionButton("print_new", "Add new print", class = "btn-primary"),
          hr(),
          textInput("print_title", "Title"),
          textAreaInput("print_description", "Description", rows = 4, width = "100%"),
          textAreaInput("print_material_notes", "Print settings / material notes",
                         rows = 3, width = "100%"),
          h4("Photos"),
          uiOutput("print_existing_photos_ui"),
          fileInput("print_photos_new", "Add photo(s)", multiple = TRUE,
                     accept = c("image/png", "image/jpeg", "image/webp")),
          h4("3D model file"),
          uiOutput("print_existing_model_ui"),
          fileInput("print_model_new", "Upload / replace model file",
                     accept = c(".stl", ".3mf", ".obj", ".step", ".stp")),
          hr(),
          actionButton("print_save", "Save print", class = "btn-success"),
          actionButton("print_delete", "Delete print", class = "btn-danger"),
          width = 5
        ),
        mainPanel(width = 7)
      )
    ),

    tabPanel("Publish",
      br(),
      helpText("Editing here only changes the source files in content/. Nothing",
                "shows up on the live site until it's rendered and pushed --",
                "that's what this tab does."),
      fluidRow(
        column(6,
          h4("1. Render"),
          helpText("Re-renders every page this editor can affect: CV, Metrics,",
                    "Recipes, Projects. The recipe encryption step only re-runs",
                    "(~30s) if a protected recipe's content actually changed."),
          actionButton("render_run", "Render site", class = "btn-primary"),
          tags$pre(style = "max-height: 300px; overflow-y: auto; background: #f8f8f8;",
                    verbatimTextOutput("render_log"))
        ),
        column(6,
          h4("2. Review & publish"),
          actionButton("git_refresh", "Check what changed"),
          tags$pre(style = "max-height: 200px; overflow-y: auto; background: #f8f8f8;",
                    verbatimTextOutput("git_status_log")),
          textAreaInput("commit_message", "Commit message",
                         value = "Update site content", rows = 2, width = "100%"),
          actionButton("publish_run", "Commit and push", class = "btn-success"),
          tags$pre(style = "max-height: 200px; overflow-y: auto; background: #f8f8f8;",
                    verbatimTextOutput("publish_log"))
        )
      )
    )
  )
)

# ---- server ---------------------------------------------------------------

server <- function(input, output, session) {

  # ============================ CV entries ==============================

  cv_data <- reactiveVal(read_cv_entries())
  cv_editing_id <- reactiveVal(NULL)

  cv_section_rows <- reactive({
    cv_data() |> filter(section == input$cv_section) |> arrange(desc(sort_year))
  })

  output$cv_table <- renderDT({
    d <- cv_section_rows() |> select(-.row_id)
    datatable(d, selection = "single", rownames = FALSE,
              options = list(pageLength = 15, dom = "tip"))
  })

  clear_cv_form <- function() {
    cv_editing_id(NULL)
    updateTextInput(session, "cv_where", value = "")
    updateTextInput(session, "cv_location", value = "")
    updateTextInput(session, "cv_year", value = "")
    updateNumericInput(session, "cv_sort_year", value = NA)
    updateTextAreaInput(session, "cv_title", value = "")
    updateCheckboxInput(session, "cv_show_full_cv", value = TRUE)
    updateCheckboxInput(session, "cv_show_onepage_cv", value = FALSE)
    updateCheckboxInput(session, "cv_show_website", value = TRUE)
  }

  observeEvent(input$cv_new, clear_cv_form())
  observeEvent(input$cv_section, clear_cv_form())

  observeEvent(input$cv_table_rows_selected, {
    sel <- input$cv_table_rows_selected
    req(sel)
    row <- cv_section_rows()[sel, ]
    cv_editing_id(row$.row_id)
    updateTextInput(session, "cv_where", value = row$where %||% "")
    updateTextInput(session, "cv_location", value = row$location %||% "")
    updateTextInput(session, "cv_year", value = row$year %||% "")
    updateNumericInput(session, "cv_sort_year", value = row$sort_year)
    updateTextAreaInput(session, "cv_title", value = row$title %||% "")
    updateCheckboxInput(session, "cv_show_full_cv", value = isTRUE(row$show_full_cv))
    updateCheckboxInput(session, "cv_show_onepage_cv", value = isTRUE(row$show_onepage_cv))
    updateCheckboxInput(session, "cv_show_website", value = isTRUE(row$show_website))
  })

  # auto-suggest sort_year from the year text (first 4-digit number found)
  observeEvent(input$cv_year, {
    m <- regmatches(input$cv_year, regexpr("[0-9]{4}", input$cv_year))
    if (length(m) == 1 && nzchar(m)) {
      updateNumericInput(session, "cv_sort_year", value = as.numeric(m))
    }
  })

  observeEvent(input$cv_save, {
    validate_msg <- NULL
    if (!nzchar(trimws(input$cv_title))) validate_msg <- "Title is required."
    if (is.na(input$cv_sort_year) && input$cv_section != "teaching") validate_msg <- "Sort year is required."
    if (!is.null(validate_msg)) { showNotification(validate_msg, type = "error"); return() }

    d <- cv_data()
    new_row <- tibble(
      section = input$cv_section,
      year = blank_to_na(input$cv_year),
      sort_year = if (is.na(input$cv_sort_year)) 0 else input$cv_sort_year,
      title = input$cv_title,
      where = if (input$cv_section %in% SECTIONS_WITH_WHERE) blank_to_na(input$cv_where) else NA_character_,
      location = if (input$cv_section %in% SECTIONS_WITH_WHERE) blank_to_na(input$cv_location) else NA_character_,
      show_full_cv = input$cv_show_full_cv,
      show_onepage_cv = input$cv_show_onepage_cv,
      show_website = input$cv_show_website
    )

    if (!is.null(cv_editing_id())) {
      new_row$.row_id <- cv_editing_id()
      d <- d |> filter(.row_id != cv_editing_id()) |> bind_rows(new_row)
    } else {
      new_row$.row_id <- if (nrow(d) == 0) 1 else max(d$.row_id) + 1
      d <- bind_rows(d, new_row)
    }
    d <- d |> arrange(.row_id)
    write_cv_entries(d)
    cv_data(d)
    clear_cv_form()
    showNotification("Entry saved.", type = "message")
  })

  observeEvent(input$cv_delete, {
    req(cv_editing_id())
    d <- cv_data() |> filter(.row_id != cv_editing_id())
    write_cv_entries(d)
    cv_data(d)
    clear_cv_form()
    showNotification("Entry deleted.", type = "message")
  })

  # ============================ Publications =============================

  pub_data <- reactiveVal(read_pubs())
  pub_editing_id <- reactiveVal(NULL)

  output$pub_table <- renderDT({
    d <- pub_data() |> select(-.row_id) |> arrange(desc(index))
    datatable(d, selection = "single", rownames = FALSE,
              options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  })

  output$pub_index_display <- renderText({
    if (is.null(pub_editing_id())) {
      d <- pub_data()
      nxt <- if (nrow(d) == 0) 1 else max(d$index) + 1
      paste("New publication will be index", nxt)
    } else {
      row <- pub_data() |> filter(.row_id == pub_editing_id())
      paste("Editing index", row$index)
    }
  })

  clear_pub_form <- function() {
    pub_editing_id(NULL)
    updateTextInput(session, "pub_year", value = "")
    updateTextAreaInput(session, "pub_authors", value = "")
    updateTextAreaInput(session, "pub_title", value = "")
    updateTextInput(session, "pub_journal", value = "")
    updateTextInput(session, "pub_volume", value = "")
    updateTextInput(session, "pub_pages", value = "")
    updateTextInput(session, "pub_doi", value = "")
    updateCheckboxInput(session, "pub_show_full_cv", value = TRUE)
    updateCheckboxInput(session, "pub_show_onepage_cv", value = FALSE)
    updateCheckboxInput(session, "pub_show_website", value = TRUE)
  }

  observeEvent(input$pub_new, clear_pub_form())

  observeEvent(input$pub_table_rows_selected, {
    sel <- input$pub_table_rows_selected
    req(sel)
    row <- (pub_data() |> arrange(desc(index)))[sel, ]
    pub_editing_id(row$.row_id)
    updateTextInput(session, "pub_year", value = row$year %||% "")
    updateTextAreaInput(session, "pub_authors", value = row$authors %||% "")
    updateTextAreaInput(session, "pub_title", value = row$title %||% "")
    updateTextInput(session, "pub_journal", value = row$journal %||% "")
    updateTextInput(session, "pub_volume", value = row$volume %||% "")
    updateTextInput(session, "pub_pages", value = row$pages %||% "")
    updateTextInput(session, "pub_doi", value = row$doi %||% "")
    updateCheckboxInput(session, "pub_show_full_cv", value = isTRUE(row$show_full_cv))
    updateCheckboxInput(session, "pub_show_onepage_cv", value = isTRUE(row$show_onepage_cv))
    updateCheckboxInput(session, "pub_show_website", value = isTRUE(row$show_website))
  })

  observeEvent(input$pub_save, {
    if (!nzchar(trimws(input$pub_title)) || !nzchar(trimws(input$pub_authors))) {
      showNotification("Title and authors are required.", type = "error"); return()
    }
    d <- pub_data()
    if (!is.null(pub_editing_id())) {
      idx <- (d |> filter(.row_id == pub_editing_id()))$index
    } else {
      idx <- if (nrow(d) == 0) 1 else max(d$index) + 1
    }
    new_row <- tibble(
      index = idx,
      year = input$pub_year,
      authors = input$pub_authors,
      title = input$pub_title,
      journal = input$pub_journal,
      volume = blank_to_na(input$pub_volume),
      pages = blank_to_na(input$pub_pages),
      doi = input$pub_doi,
      show_full_cv = input$pub_show_full_cv,
      show_onepage_cv = input$pub_show_onepage_cv,
      show_website = input$pub_show_website
    )
    if (!is.null(pub_editing_id())) {
      new_row$.row_id <- pub_editing_id()
      d <- d |> filter(.row_id != pub_editing_id()) |> bind_rows(new_row)
    } else {
      new_row$.row_id <- if (nrow(d) == 0) 1 else max(d$.row_id) + 1
      d <- bind_rows(d, new_row)
    }
    d <- d |> arrange(.row_id)
    write_pubs(d)
    pub_data(d)
    clear_pub_form()
    showNotification("Publication saved.", type = "message")
  })

  observeEvent(input$pub_delete, {
    req(pub_editing_id())
    d <- pub_data() |> filter(.row_id != pub_editing_id())
    write_pubs(d)
    pub_data(d)
    clear_pub_form()
    showNotification("Publication deleted.", type = "message")
  })

  # ============================ Profile text ==============================

  meta <- reactiveVal(yaml::read_yaml(META_PATH))

  observe({
    m <- meta()
    updateTextInput(session, "meta_name", value = m$name %||% "")
    updateTextInput(session, "meta_title", value = m$title %||% "")
    updateTextInput(session, "meta_position", value = m$position %||% "")
    updateTextInput(session, "meta_employer", value = m$employer %||% "")
    updateTextInput(session, "meta_email", value = m$email %||% "")
    updateTextInput(session, "meta_website", value = m$website %||% "")
    updateTextInput(session, "meta_orcid", value = m$orcid %||% "")
    updateTextInput(session, "meta_orcid_url", value = m$orcid_url %||% "")
    updateTextInput(session, "meta_scholar_id", value = m$scholar_id %||% "")
    updateTextInput(session, "meta_scholar_url", value = m$scholar_url %||% "")
    updateTextInput(session, "meta_github", value = m$github %||% "")
    updateTextInput(session, "meta_bluesky", value = m$bluesky %||% "")
    updateTextInput(session, "meta_linkedin", value = m$linkedin %||% "")
    updateTextAreaInput(session, "meta_speciality", value = trimws(m$speciality %||% ""))
    updateTextAreaInput(session, "meta_peer_review", value = paste(unlist(m$peer_review), collapse = "\n"))
  })

  observeEvent(input$meta_save, {
    new_meta <- list(
      name = input$meta_name, title = input$meta_title, position = input$meta_position,
      employer = input$meta_employer, email = input$meta_email, website = input$meta_website,
      scholar_id = input$meta_scholar_id, scholar_url = input$meta_scholar_url,
      github = input$meta_github, bluesky = input$meta_bluesky, linkedin = input$meta_linkedin,
      orcid = input$meta_orcid, orcid_url = input$meta_orcid_url,
      speciality = input$meta_speciality,
      peer_review = as.list(Filter(nzchar, strsplit(input$meta_peer_review, "\n")[[1]]))
    )
    yaml::write_yaml(new_meta, META_PATH)
    meta(new_meta)
    showNotification("Profile text saved.", type = "message")
  })

  # ============================ Recipes ==================================

  recipes_data <- reactiveVal(yaml::read_yaml(RECIPES_PATH))
  ingredient_ids <- reactiveVal(integer(0))
  # Seed values for ingredient rows, keyed by as.character(id) -- read by
  # renderUI() when it *creates* a row. updateTextInput() etc only work on
  # inputs that already exist client-side, which a freshly (re)created row
  # doesn't yet, so new rows must get their starting value baked into the
  # textInput()/numericInput() call itself rather than updated afterwards.
  ingredient_initial <- reactiveVal(list())
  ingredient_counter <- reactiveVal(0)
  editing_slug <- reactiveVal(NULL)

  # Protected recipes' ingredients/instructions live outside recipes_data()
  # entirely (see content/recipes_protected.yml, merged in only at
  # quarto-render time) -- this editor never has their real content to show,
  # and saving one from here would silently overwrite it with an empty
  # ingredient list and drop the `protected: yes` flag. Excluding them from
  # the dropdown is what actually prevents that, not just a warning label.
  editable_recipes <- reactive(Filter(function(r) !isTRUE(r$protected), recipes_data()))

  update_recipe_choices <- function(select = NULL) {
    names_list <- vapply(editable_recipes(), function(r) r$name, character(1))
    slugs_list <- vapply(editable_recipes(), function(r) r$slug, character(1))
    choices <- setNames(slugs_list, names_list)
    updateSelectInput(session, "recipe_select", choices = choices, selected = select)
  }
  observe(update_recipe_choices())

  set_ingredient_rows <- function(ingredients) {
    n <- length(ingredients)
    ids <- if (n == 0) integer(0) else (ingredient_counter() + 1):(ingredient_counter() + n)
    ingredient_counter(ingredient_counter() + n)
    vals <- stats::setNames(
      lapply(ingredients, function(i) list(name = i$name %||% "", amount = i$amount %||% 0, unit = i$unit %||% "")),
      as.character(ids)
    )
    ingredient_initial(vals)
    ingredient_ids(ids)
  }

  output$ingredient_rows <- renderUI({
    ids <- ingredient_ids()
    if (length(ids) == 0) return(p("No ingredients yet -- click \"Add ingredient\"."))
    vals <- isolate(ingredient_initial())
    tagList(lapply(ids, function(id) {
      v <- vals[[as.character(id)]] %||% list(name = "", amount = 0, unit = "")
      fluidRow(
        column(5, textInput(paste0("ing_name_", id), NULL, value = v$name, placeholder = "Name")),
        column(3, numericInput(paste0("ing_amount_", id), NULL, value = v$amount)),
        column(3, textInput(paste0("ing_unit_", id), NULL, value = v$unit, placeholder = "Unit")),
        column(1, actionButton(paste0("ing_remove_", id), "✕"))
      )
    }))
  })

  observeEvent(input$ingredient_add, {
    id <- ingredient_counter() + 1
    ingredient_counter(id)
    ingredient_initial(c(ingredient_initial(), stats::setNames(list(list(name = "", amount = 0, unit = "")), as.character(id))))
    ingredient_ids(c(ingredient_ids(), id))
  })

  observe({
    lapply(ingredient_ids(), function(id) {
      btn <- paste0("ing_remove_", id)
      observeEvent(input[[btn]], {
        ingredient_ids(setdiff(ingredient_ids(), id))
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  clear_recipe_form <- function() {
    editing_slug(NULL)
    updateTextInput(session, "recipe_name", value = "")
    updateTextInput(session, "recipe_slug", value = "")
    updateNumericInput(session, "recipe_servings", value = 4)
    updateTextInput(session, "recipe_source_name", value = "")
    updateTextInput(session, "recipe_source_url", value = "")
    updateTextAreaInput(session, "recipe_instructions", value = "")
    set_ingredient_rows(list())
  }

  observeEvent(input$recipe_new, {
    clear_recipe_form()
    updateSelectInput(session, "recipe_select", selected = character(0))
  })

  # auto-fill slug from name, only while adding a brand-new recipe
  observeEvent(input$recipe_name, {
    if (is.null(editing_slug())) {
      slug <- tolower(gsub("[^a-z0-9]+", "_", tolower(input$recipe_name)))
      slug <- gsub("^_|_$", "", slug)
      updateTextInput(session, "recipe_slug", value = slug)
    }
  })

  observeEvent(input$recipe_select, {
    req(input$recipe_select)
    r <- Filter(function(x) x$slug == input$recipe_select, recipes_data())
    req(length(r) == 1)
    r <- r[[1]]
    editing_slug(r$slug)
    updateTextInput(session, "recipe_name", value = r$name)
    updateTextInput(session, "recipe_slug", value = r$slug)
    updateNumericInput(session, "recipe_servings", value = r$base_servings)
    updateTextInput(session, "recipe_source_name", value = r$source_name %||% "")
    updateTextInput(session, "recipe_source_url", value = r$source_url %||% "")
    updateTextAreaInput(session, "recipe_instructions", value = trimws(r$instructions))
    set_ingredient_rows(r$ingredients)
  })

  current_ingredients <- reactive({
    ids <- ingredient_ids()
    lapply(ids, function(id) {
      list(
        name = input[[paste0("ing_name_", id)]] %||% "",
        amount = input[[paste0("ing_amount_", id)]] %||% 0,
        unit = input[[paste0("ing_unit_", id)]] %||% ""
      )
    })
  })

  output$recipe_preview <- renderTable({
    ings <- current_ingredients()
    req(length(ings) > 0)
    do.call(rbind, lapply(ings, as.data.frame))
  })

  observeEvent(input$recipe_save, {
    if (!nzchar(trimws(input$recipe_name)) || !nzchar(trimws(input$recipe_slug))) {
      showNotification("Name and slug are required.", type = "error"); return()
    }
    ings <- current_ingredients()
    if (length(ings) == 0 || any(!vapply(ings, function(i) nzchar(i$name), logical(1)))) {
      showNotification("Every ingredient needs a name.", type = "error"); return()
    }
    new_recipe <- list(
      name = input$recipe_name, slug = input$recipe_slug,
      base_servings = input$recipe_servings, instructions = input$recipe_instructions,
      ingredients = ings
    )
    if (nzchar(trimws(input$recipe_source_name))) new_recipe$source_name <- input$recipe_source_name
    if (nzchar(trimws(input$recipe_source_url))) new_recipe$source_url <- input$recipe_source_url
    all_recipes <- recipes_data()
    other_slugs <- vapply(all_recipes, function(r) r$slug, character(1))
    if (is.null(editing_slug())) {
      if (input$recipe_slug %in% other_slugs) {
        showNotification("That slug is already used by another recipe.", type = "error"); return()
      }
      all_recipes <- c(all_recipes, list(new_recipe))
    } else {
      keep <- other_slugs != editing_slug()
      if (input$recipe_slug != editing_slug() && input$recipe_slug %in% other_slugs[keep]) {
        showNotification("That slug is already used by another recipe.", type = "error"); return()
      }
      all_recipes <- c(Filter(function(r) r$slug != editing_slug(), all_recipes), list(new_recipe))
    }
    yaml::write_yaml(all_recipes, RECIPES_PATH)
    recipes_data(all_recipes)
    editing_slug(input$recipe_slug)
    update_recipe_choices(select = input$recipe_slug)
    showNotification("Recipe saved.", type = "message")
  })

  observeEvent(input$recipe_delete, {
    req(editing_slug())
    all_recipes <- Filter(function(r) r$slug != editing_slug(), recipes_data())
    yaml::write_yaml(all_recipes, RECIPES_PATH)
    recipes_data(all_recipes)
    clear_recipe_form()
    update_recipe_choices()
    showNotification("Recipe deleted.", type = "message")
  })

  # ============================ Locked recipes ============================
  # Mirrors the Recipes tab above, but for protected == TRUE entries. Public
  # metadata (name/slug/servings/source) still goes to the tracked
  # RECIPES_PATH -- shared with the public Recipes tab via the same
  # recipes_data() reactive, so either tab saving keeps both dropdowns in
  # sync. Only ingredients/instructions go to the gitignored
  # PROTECTED_RECIPES_PATH, keyed by slug.

  read_protected_content <- function() {
    if (file.exists(PROTECTED_RECIPES_PATH)) yaml::read_yaml(PROTECTED_RECIPES_PATH) else list()
  }

  locked_content_data <- reactiveVal(read_protected_content())
  locked_ingredient_ids <- reactiveVal(integer(0))
  locked_ingredient_initial <- reactiveVal(list())
  locked_ingredient_counter <- reactiveVal(0)
  locked_editing_slug <- reactiveVal(NULL)

  locked_recipes <- reactive(Filter(function(r) isTRUE(r$protected), recipes_data()))

  update_locked_recipe_choices <- function(select = NULL) {
    names_list <- vapply(locked_recipes(), function(r) r$name, character(1))
    slugs_list <- vapply(locked_recipes(), function(r) r$slug, character(1))
    choices <- setNames(slugs_list, names_list)
    updateSelectInput(session, "locked_recipe_select", choices = choices, selected = select)
  }
  observe(update_locked_recipe_choices())

  set_locked_ingredient_rows <- function(ingredients) {
    n <- length(ingredients)
    ids <- if (n == 0) integer(0) else (locked_ingredient_counter() + 1):(locked_ingredient_counter() + n)
    locked_ingredient_counter(locked_ingredient_counter() + n)
    vals <- stats::setNames(
      lapply(ingredients, function(i) list(name = i$name %||% "", amount = i$amount %||% 0, unit = i$unit %||% "")),
      as.character(ids)
    )
    locked_ingredient_initial(vals)
    locked_ingredient_ids(ids)
  }

  output$locked_ingredient_rows <- renderUI({
    ids <- locked_ingredient_ids()
    if (length(ids) == 0) return(p("No ingredients yet -- click \"Add ingredient\"."))
    vals <- isolate(locked_ingredient_initial())
    tagList(lapply(ids, function(id) {
      v <- vals[[as.character(id)]] %||% list(name = "", amount = 0, unit = "")
      fluidRow(
        column(5, textInput(paste0("locked_ing_name_", id), NULL, value = v$name, placeholder = "Name")),
        column(3, numericInput(paste0("locked_ing_amount_", id), NULL, value = v$amount)),
        column(3, textInput(paste0("locked_ing_unit_", id), NULL, value = v$unit, placeholder = "Unit")),
        column(1, actionButton(paste0("locked_ing_remove_", id), "✕"))
      )
    }))
  })

  observeEvent(input$locked_ingredient_add, {
    id <- locked_ingredient_counter() + 1
    locked_ingredient_counter(id)
    locked_ingredient_initial(c(locked_ingredient_initial(), stats::setNames(list(list(name = "", amount = 0, unit = "")), as.character(id))))
    locked_ingredient_ids(c(locked_ingredient_ids(), id))
  })

  observe({
    lapply(locked_ingredient_ids(), function(id) {
      btn <- paste0("locked_ing_remove_", id)
      observeEvent(input[[btn]], {
        locked_ingredient_ids(setdiff(locked_ingredient_ids(), id))
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  clear_locked_recipe_form <- function() {
    locked_editing_slug(NULL)
    updateTextInput(session, "locked_recipe_name", value = "")
    updateTextInput(session, "locked_recipe_slug", value = "")
    updateNumericInput(session, "locked_recipe_servings", value = 4)
    updateTextInput(session, "locked_recipe_source_name", value = "")
    updateTextInput(session, "locked_recipe_source_url", value = "")
    updateTextAreaInput(session, "locked_recipe_instructions", value = "")
    set_locked_ingredient_rows(list())
  }

  observeEvent(input$locked_recipe_new, {
    clear_locked_recipe_form()
    updateSelectInput(session, "locked_recipe_select", selected = character(0))
  })

  observeEvent(input$locked_recipe_name, {
    if (is.null(locked_editing_slug())) {
      slug <- tolower(gsub("[^a-z0-9]+", "_", tolower(input$locked_recipe_name)))
      slug <- gsub("^_|_$", "", slug)
      updateTextInput(session, "locked_recipe_slug", value = slug)
    }
  })

  observeEvent(input$locked_recipe_select, {
    req(input$locked_recipe_select)
    r <- Filter(function(x) x$slug == input$locked_recipe_select, locked_recipes())
    req(length(r) == 1)
    r <- r[[1]]
    locked_editing_slug(r$slug)
    updateTextInput(session, "locked_recipe_name", value = r$name)
    updateTextInput(session, "locked_recipe_slug", value = r$slug)
    updateNumericInput(session, "locked_recipe_servings", value = r$base_servings)
    updateTextInput(session, "locked_recipe_source_name", value = r$source_name %||% "")
    updateTextInput(session, "locked_recipe_source_url", value = r$source_url %||% "")
    content <- locked_content_data()[[r$slug]]
    updateTextAreaInput(session, "locked_recipe_instructions", value = trimws(content$instructions %||% ""))
    set_locked_ingredient_rows(content$ingredients)
  })

  current_locked_ingredients <- reactive({
    ids <- locked_ingredient_ids()
    lapply(ids, function(id) {
      list(
        name = input[[paste0("locked_ing_name_", id)]] %||% "",
        amount = input[[paste0("locked_ing_amount_", id)]] %||% 0,
        unit = input[[paste0("locked_ing_unit_", id)]] %||% ""
      )
    })
  })

  output$locked_recipe_preview <- renderTable({
    ings <- current_locked_ingredients()
    req(length(ings) > 0)
    do.call(rbind, lapply(ings, as.data.frame))
  })

  observeEvent(input$locked_recipe_save, {
    if (!nzchar(trimws(input$locked_recipe_name)) || !nzchar(trimws(input$locked_recipe_slug))) {
      showNotification("Name and slug are required.", type = "error"); return()
    }
    ings <- current_locked_ingredients()
    if (length(ings) == 0 || any(!vapply(ings, function(i) nzchar(i$name), logical(1)))) {
      showNotification("Every ingredient needs a name.", type = "error"); return()
    }
    slug <- input$locked_recipe_slug

    new_meta <- list(
      name = input$locked_recipe_name, slug = slug,
      base_servings = input$locked_recipe_servings, protected = TRUE
    )
    if (nzchar(trimws(input$locked_recipe_source_name))) new_meta$source_name <- input$locked_recipe_source_name
    if (nzchar(trimws(input$locked_recipe_source_url))) new_meta$source_url <- input$locked_recipe_source_url

    all_recipes <- recipes_data()
    other_slugs <- vapply(all_recipes, function(r) r$slug, character(1))
    if (is.null(locked_editing_slug())) {
      if (slug %in% other_slugs) {
        showNotification("That slug is already used by another recipe.", type = "error"); return()
      }
      all_recipes <- c(all_recipes, list(new_meta))
    } else {
      keep <- other_slugs != locked_editing_slug()
      if (slug != locked_editing_slug() && slug %in% other_slugs[keep]) {
        showNotification("That slug is already used by another recipe.", type = "error"); return()
      }
      all_recipes <- c(Filter(function(r) r$slug != locked_editing_slug(), all_recipes), list(new_meta))
    }
    yaml::write_yaml(all_recipes, RECIPES_PATH)
    recipes_data(all_recipes)

    content <- locked_content_data()
    if (!is.null(locked_editing_slug()) && locked_editing_slug() != slug) {
      content[[locked_editing_slug()]] <- NULL
    }
    content[[slug]] <- list(instructions = input$locked_recipe_instructions, ingredients = ings)
    yaml::write_yaml(content, PROTECTED_RECIPES_PATH)
    locked_content_data(content)

    locked_editing_slug(slug)
    update_locked_recipe_choices(select = slug)
    showNotification("Locked recipe saved.", type = "message")
  })

  observeEvent(input$locked_recipe_delete, {
    req(locked_editing_slug())
    all_recipes <- Filter(function(r) r$slug != locked_editing_slug(), recipes_data())
    yaml::write_yaml(all_recipes, RECIPES_PATH)
    recipes_data(all_recipes)

    content <- locked_content_data()
    content[[locked_editing_slug()]] <- NULL
    yaml::write_yaml(content, PROTECTED_RECIPES_PATH)
    locked_content_data(content)

    clear_locked_recipe_form()
    update_locked_recipe_choices()
    showNotification("Locked recipe deleted.", type = "message")
  })

  # ============================ Projects ==================================

  projects_data <- reactiveVal(yaml::read_yaml(PROJECTS_PATH))
  project_editing_idx <- reactiveVal(NULL)

  update_project_choices <- function(select = NULL) {
    titles <- vapply(projects_data(), function(p) p$title, character(1))
    choices <- stats::setNames(seq_along(titles), titles)
    updateSelectInput(session, "project_select", choices = choices, selected = select)
  }
  observe(update_project_choices())

  clear_project_form <- function() {
    project_editing_idx(NULL)
    updateTextInput(session, "project_title", value = "")
    updateTextAreaInput(session, "project_body", value = "")
  }

  observeEvent(input$project_new, {
    clear_project_form()
    updateSelectInput(session, "project_select", selected = character(0))
  })

  observeEvent(input$project_select, {
    req(input$project_select)
    idx <- as.integer(input$project_select)
    p <- projects_data()[[idx]]
    req(!is.null(p))
    project_editing_idx(idx)
    updateTextInput(session, "project_title", value = p$title)
    updateTextAreaInput(session, "project_body", value = trimws(p$body))
  })

  observeEvent(input$project_save, {
    if (!nzchar(trimws(input$project_title)) || !nzchar(trimws(input$project_body))) {
      showNotification("Title and body are required.", type = "error"); return()
    }
    new_project <- list(title = input$project_title, body = input$project_body)
    all_projects <- projects_data()
    if (is.null(project_editing_idx())) {
      all_projects <- c(all_projects, list(new_project))
      new_idx <- length(all_projects)
    } else {
      all_projects[[project_editing_idx()]] <- new_project
      new_idx <- project_editing_idx()
    }
    yaml::write_yaml(all_projects, PROJECTS_PATH)
    projects_data(all_projects)
    project_editing_idx(new_idx)
    update_project_choices(select = new_idx)
    showNotification("Project saved.", type = "message")
  })

  observeEvent(input$project_delete, {
    req(project_editing_idx())
    all_projects <- projects_data()
    all_projects[[project_editing_idx()]] <- NULL
    yaml::write_yaml(all_projects, PROJECTS_PATH)
    projects_data(all_projects)
    clear_project_form()
    update_project_choices()
    showNotification("Project deleted.", type = "message")
  })

  # ============================ 3D Prints ================================

  prints_data <- reactiveVal(yaml::read_yaml(PRINTS_PATH))
  print_editing_slug <- reactiveVal(NULL)

  update_print_choices <- function(select = NULL) {
    all <- prints_data()
    names_list <- vapply(all, function(p) p$title, character(1))
    slugs_list <- vapply(all, function(p) p$slug, character(1))
    choices <- setNames(slugs_list, names_list)
    updateSelectInput(session, "print_select", choices = choices, selected = select)
  }
  observe(update_print_choices())

  find_print <- function(slug) {
    if (is.null(slug)) return(NULL)
    p <- Filter(function(x) x$slug == slug, prints_data())
    if (length(p) == 1) p[[1]] else NULL
  }

  clear_print_form <- function() {
    print_editing_slug(NULL)
    updateTextInput(session, "print_title", value = "")
    updateTextAreaInput(session, "print_description", value = "")
    updateTextAreaInput(session, "print_material_notes", value = "")
  }

  observeEvent(input$print_new, {
    clear_print_form()
    updateSelectInput(session, "print_select", selected = character(0))
  })

  observeEvent(input$print_select, {
    req(input$print_select)
    p <- find_print(input$print_select)
    req(!is.null(p))
    print_editing_slug(p$slug)
    updateTextInput(session, "print_title", value = p$title)
    updateTextAreaInput(session, "print_description", value = trimws(p$description %||% ""))
    updateTextAreaInput(session, "print_material_notes", value = trimws(p$material_notes %||% ""))
  })

  output$print_existing_photos_ui <- renderUI({
    p <- find_print(print_editing_slug())
    photos <- unlist(p$photos)
    if (is.null(p) || length(photos) == 0) return(helpText("No photos yet."))
    choices <- setNames(photos, basename(photos))
    checkboxGroupInput("print_keep_photos", NULL, choices = choices, selected = photos)
  })

  output$print_existing_model_ui <- renderUI({
    p <- find_print(print_editing_slug())
    if (is.null(p) || is.null(p$model_file)) return(helpText("No model file yet."))
    checkboxInput("print_keep_model",
                   paste0("Keep existing: ", p$model_filename %||% basename(p$model_file)),
                   value = TRUE)
  })

  observeEvent(input$print_save, {
    if (!nzchar(trimws(input$print_title))) {
      showNotification("Title is required.", type = "error"); return()
    }
    all_prints <- prints_data()
    is_new <- is.null(print_editing_slug())

    if (is_new) {
      slug <- gsub("^_|_$", "", tolower(gsub("[^a-z0-9]+", "_", tolower(input$print_title))))
      other_slugs <- vapply(all_prints, function(p) p$slug, character(1))
      if (slug %in% other_slugs) {
        showNotification("A print with that (or a very similar) title already exists.", type = "error")
        return()
      }
    } else {
      slug <- print_editing_slug()
    }

    dest_dir <- file.path(PRINTS_UPLOADS_DIR, slug)
    dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    existing <- find_print(slug)

    # Photos: anything left unchecked gets deleted from disk; anything
    # newly uploaded gets copied in under its own (de-duplicated) name.
    existing_photos <- unlist(existing$photos)
    kept_photos <- input$print_keep_photos %||% character(0)
    for (rel in setdiff(existing_photos, kept_photos)) {
      f <- file.path(PROJECT_ROOT, rel)
      if (file.exists(f)) file.remove(f)
    }
    photo_paths <- kept_photos
    if (!is.null(input$print_photos_new)) {
      up <- input$print_photos_new
      for (i in seq_len(nrow(up))) {
        dest_name <- make_unique_filename(dest_dir, up$name[i])
        file.copy(up$datapath[i], file.path(dest_dir, dest_name), overwrite = TRUE)
        photo_paths <- c(photo_paths, file.path("prints_uploads", slug, dest_name))
      }
    }

    # Model file: dropped if unchecked, replaced if a new one is uploaded,
    # otherwise left as-is.
    keep_model <- isTRUE(input$print_keep_model)
    model_file <- existing$model_file
    model_filename <- existing$model_filename
    if (!is.null(model_file) && !keep_model) {
      f <- file.path(PROJECT_ROOT, model_file)
      if (file.exists(f)) file.remove(f)
      model_file <- NULL; model_filename <- NULL
    }
    if (!is.null(input$print_model_new) && nzchar(input$print_model_new$name)) {
      if (!is.null(model_file)) {
        f <- file.path(PROJECT_ROOT, model_file)
        if (file.exists(f)) file.remove(f)
      }
      dest_name <- make_unique_filename(dest_dir, input$print_model_new$name)
      file.copy(input$print_model_new$datapath, file.path(dest_dir, dest_name), overwrite = TRUE)
      model_file <- file.path("prints_uploads", slug, dest_name)
      model_filename <- input$print_model_new$name
    }

    new_entry <- list(
      title = input$print_title, slug = slug,
      description = input$print_description,
      photos = as.list(photo_paths)
    )
    if (nzchar(trimws(input$print_material_notes))) new_entry$material_notes <- input$print_material_notes
    if (!is.null(model_file)) {
      new_entry$model_file <- model_file
      new_entry$model_filename <- model_filename
    }

    all_prints <- if (is_new) {
      c(all_prints, list(new_entry))
    } else {
      lapply(all_prints, function(p) if (p$slug == slug) new_entry else p)
    }
    yaml::write_yaml(all_prints, PRINTS_PATH)
    prints_data(all_prints)
    print_editing_slug(slug)
    update_print_choices(select = slug)
    showNotification("Print saved.", type = "message")
  })

  observeEvent(input$print_delete, {
    req(print_editing_slug())
    slug <- print_editing_slug()
    all_prints <- Filter(function(p) p$slug != slug, prints_data())
    yaml::write_yaml(all_prints, PRINTS_PATH)
    prints_data(all_prints)
    dest_dir <- file.path(PRINTS_UPLOADS_DIR, slug)
    if (dir.exists(dest_dir)) unlink(dest_dir, recursive = TRUE)
    clear_print_form()
    update_print_choices()
    showNotification("Print deleted.", type = "message")
  })

  # ============================ Publish ==================================

  # Absolute paths passed to quarto/git rather than setwd()-ing -- this
  # app's own working directory is admin/ (see PROJECT_ROOT above), and
  # `quarto render <path>` / `git -C <path>` both work from anywhere,
  # so there's no need to juggle the process's cwd mid-session.
  RENDER_TARGETS <- c("cv.qmd", "metrics.qmd", "recipes.qmd", "projects.qmd", "lab/prints.qmd")
  # Everything this editor can write to. docs/ covers every rendered
  # output file; prints_uploads/ holds the actual photo/model files (the
  # lab/prints.qmd render step copies them into docs/ itself, but the
  # source copies here still need to reach git); content/recipes_protected.yml
  # is deliberately excluded since it's gitignored (see recipes.qmd) and
  # `git add` on a path git already ignores is a silent no-op, not an error.
  PUBLISH_PATHS <- c(
    "content/cv_entries.csv", "content/pubs.csv", "content/cv_meta.yml",
    "content/recipes.yml", "content/projects.yml", "content/prints.yml",
    "prints_uploads", "docs"
  )

  render_log <- reactiveVal("Click \"Render site\" to rebuild the pages this editor can affect.")
  git_status_log <- reactiveVal("Click \"Check what changed\" to see the pending diff.")
  publish_log <- reactiveVal("")

  output$render_log <- renderText(render_log())
  output$git_status_log <- renderText(git_status_log())
  output$publish_log <- renderText(publish_log())

  refresh_git_status <- function() {
    out <- system2("git", c("-C", PROJECT_ROOT, "status", "--short"), stdout = TRUE, stderr = TRUE)
    git_status_log(if (length(out) == 0) {
      "Nothing to publish -- working tree matches the last commit."
    } else {
      paste(out, collapse = "\n")
    })
  }

  observeEvent(input$render_run, {
    render_log("Rendering...")
    chunks <- lapply(RENDER_TARGETS, function(page) {
      path <- file.path(PROJECT_ROOT, page)
      out <- system2("quarto", c("render", path), stdout = TRUE, stderr = TRUE)
      status <- attr(out, "status") %||% 0
      paste0(
        "== ", page, if (!identical(status, 0L) && !identical(status, 0)) " (FAILED)" else "", " ==\n",
        paste(out, collapse = "\n")
      )
    })
    render_log(paste(chunks, collapse = "\n\n"))
    refresh_git_status()
    showNotification("Render finished -- check the log for errors before publishing.", type = "message")
  })

  observeEvent(input$git_refresh, refresh_git_status())

  observeEvent(input$publish_run, {
    if (!nzchar(trimws(input$commit_message))) {
      showNotification("Commit message is required.", type = "error"); return()
    }
    existing_paths <- PUBLISH_PATHS[file.exists(file.path(PROJECT_ROOT, PUBLISH_PATHS))]
    add_out <- system2("git", c("-C", PROJECT_ROOT, "add", existing_paths), stdout = TRUE, stderr = TRUE)

    staged <- system2("git", c("-C", PROJECT_ROOT, "diff", "--cached", "--stat"), stdout = TRUE, stderr = TRUE)
    if (length(staged) == 0) {
      publish_log("Nothing staged -- nothing to commit.")
      showNotification("Nothing to publish.", type = "warning")
      return()
    }

    commit_out <- system2(
      "git", c("-C", PROJECT_ROOT, "commit", "-m", shQuote(input$commit_message)),
      stdout = TRUE, stderr = TRUE
    )
    push_out <- system2("git", c("-C", PROJECT_ROOT, "push"), stdout = TRUE, stderr = TRUE)

    publish_log(paste(
      c("== staged ==", staged, "", "== commit ==", commit_out, "", "== push ==", push_out),
      collapse = "\n"
    ))
    refresh_git_status()
    showNotification("Published.", type = "message")
  })

}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

shinyApp(ui, server)
