library(shiny)

shinyUI(
  fixedPage(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "pulse"
    ),
    waiter::use_waiter(),
    titlePanel("QFieldCloud Admin"),

    # selected project
    fluidRow(
      class = "border m-2 p-2",
      fluidRow(
        column(
          4,
          h4("Login"),
          # QFieldCloud login
          textInput(
            "qfieldcloud_username",
            "QFieldCloud email:",
            value = "",
            placeholder = ""
          )
        ),
        column(
          8,
          h4("Projects"),
          fluidRow(
            column(
              6,
              selectInput(
                "manager_projects",
                "Manager / admin projects:",
                choices = ""
              )
            ),
            column(
              6,
              selectInput(
                "other_projects",
                "Collaborator projects:",
                choices = ""
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          4,
          # QFieldCloud password
          passwordInput(
            "qfieldcloud_password",
            "QFieldCloud password:",
            value = "",
            placeholder = ""
          )
        )
      ),
      fluidRow(
        column(
          4,
          tags$div(
            class="d-grid gap-2",
            actionButton(
              class = "btn btn-primary",
              "login",
              "login"
            ),
            uiOutput("login_status")
          )
        ),
        column(
          4,
          uiOutput("change_password")
        ),
        column(
          4,
          tags$div(
            class="d-grid gap-2",
            tags$a(
              class = "btn btn-primary action-button",
              "forgot password",
              target = "_blank",
              href = paste0("https://", qfieldcloud_url, "/accounts/password/reset/")
            )
          )
        )
      )
    ),
    # selected project
    fluidRow(
      class = "border m-2 p-2",
      fluidRow(
        column(
          12,
          h2("Project Management")
        )
      ),
      fluidRow(
        class = "dt-scroll",
        column(
          4,
          h6("Project name:"),
          textOutput("project_name"),
          h6("Number of admin / managers:"),
          textOutput("admin_count"),
          h6("Number of collaborators:"),
          textOutput("collabs_count"),
        ),
        column(
          8,
          h6("Project files:"),
          downloadButton(
            "download_project",
            "download project",
            class = "btn-primary m-2"
          ),
          div(
            DT::dataTableOutput("project_files")
          )
        )
      ),
      hr(
        class = "m-4"
      ),

      # manager users projects
      fluidRow(
        column(
          4,
          selectInput(
            "action_type",
            "action_type",
            choices = c(
              "add",
              "delete",
              "update"
            )
          ),
          conditionalPanel(
            condition = "input.action_type == 'add' | input.action_type == 'update'",
            selectInput(
              "user_role",
              "user role",
              choices = c(
                "reporter",
                "reader",
                "editor",
                "manager",
                "admin"
              )
            )
          ),
          actionButton(
            "apply_action",
            "apply changes"
          )
        ),
        column(
          4,
          selectInput(
            "project_collaborators",
            "Project collaborators",
            choices = "",
            multiple = TRUE
          )
        ),
        column(
          4,
          selectInput(
            "potential_users",
            "Available collaborators",
            choices = "",
            multiple = TRUE
          )
        )
      )
    )
  )
)
