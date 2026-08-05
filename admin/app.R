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
          helpText("Password-protected recipes aren't editable here -- their",
                    "ingredients/instructions live in the gitignored",
                    "content/recipes_protected.yml, edited by hand. This list",
                    "only shows the ones safe to store in the public repo."),
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

}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

shinyApp(ui, server)
