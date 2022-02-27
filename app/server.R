library(shiny)

shinyServer(function(input, output, session) {
  app_data <- reactiveValues(
    token = NULL,
    manager_projects = NULL,
    other_projects = NULL,
    project_files = NULL,
    collaborators = NULL,
    users = NULL
  )

  # waiting screen
  waiter <- waiter::Waiter$new(
    html = logging_in_screen,
    color = "rgba(89,49,150,.6)"
  )
  
  download_waiter <- waiter::Waiter$new(
    html = download_screen,
    color = "rgba(89,49,150,.6)"
  )
  
  update_waiter <- waiter::Waiter$new(
    html = updating_screen,
    color = "rgba(89,49,150,.6)"
  )
  
  add_waiter <- waiter::Waiter$new(
    html = adding_screen,
    color = "rgba(89,49,150,.6)"
  )
  
  delete_waiter <- waiter::Waiter$new(
    html = deleting_screen,
    color = "rgba(89,49,150,.6)"
  )
  
  updating_user_waiter <- waiter::Waiter$new(
    html = updating_user_status_screen,
    color = "rgba(89,49,150,.6)"
  )

  # Login -------------------------------------------------------------------

  # token is TRUE if the user is logged in successfully
  # get QFieldCloud token
  observeEvent(input$login, {
    username <- input$qfieldcloud_username
    password <- input$qfieldcloud_password
    endpoint <- qfieldcloud_url

    token <- qfieldcloudR::qfieldcloud_login(
      username,
      password,
      endpoint
    )
    
    waiter$show()

    if (token$status == "success") {
      app_data$token <- token$token

      login_message <- paste0("logged in as ", username)

      output$login_status <- renderUI({
        tags$p(login_message)
      })

      output$change_password <- renderUI({
        tags$div(
          class="d-grid gap-2",
          tags$a(
            class = "btn btn-primary action-button",
            "change password",
            target = "_blank",
            href = paste0("https://", qfieldcloud_url, "/accounts/password/change/")
          )
        )
      })
    } else {
      app_data$token <- NULL
      login_message <-
        paste0("login failed - check username and password")
      output$login_status <- renderUI({
        tags$p(login_message)
      })
    }
    
    waiter$hide()
  })


  # Get and Select Project --------------------------------------------------

  # select one layer as active layer from S3 GeoPackages
  observe({
    req(app_data$token)

    # get user's projects
    if (!is.null(app_data$token)) {
      users_projects <- qfieldcloudR::get_qfieldcloud_projects(
        app_data$token,
        qfieldcloud_url
      )
    }

    if ("data.frame" %in% class(users_projects)) {
      tryCatch(
        error = function(cnd) {
          showNotification("Could not admin / manager projects.", type = "error")
        },
        {
          manager_projects <- users_projects %>%
            dplyr::filter(user_role == "admin" | user_role == "manager")

          app_data$manager_projects <- manager_projects
        }
      )

      tryCatch(
        error = function(cnd) {
          showNotification("Could not load projects.", type = "error")
        },
        {
          other_projects <- users_projects %>%
            dplyr::filter(user_role != "admin" & user_role != "manager")

          app_data$other_projects <- other_projects
        }
      )
    }

    if (!is.null(app_data$manager_projects) & nrow(app_data$manager_projects) > 0) {
      choices <- try(app_data$manager_projects$name)
      if (!("try-error" %in% class(choices))) {
        updateSelectInput(
          session,
          "manager_projects",
          choices = choices
        )
      }
    }

    if (!is.null(app_data$other_projects) & nrow(app_data$other_projects) > 0) {
      choices <- try(app_data$other_projects$name)
      if (!("try-error" %in% class(choices))) {
        updateSelectInput(
          session,
          "other_projects",
          choices = choices
        )
      }
    }
  })

  # Selected project --------------------------------------------------------

  observeEvent(input$manager_projects, {
    req(input$manager_projects)

    download_waiter$show()

    # get project info
    tryCatch(
      error = function(cnd) {
        showNotification("Could not load project details.", type = "error")
      },
      {
        project_id <- app_data$manager_projects %>%
          dplyr::filter(name == input$manager_projects) %>%
          dplyr::select(id)

        project_id <- project_id$id

        project_name <- input$manager_projects

        output$project_name <- renderText({
          project_name
        })

        # get project collaborators
        collabs <- qfieldcloudR::get_qfieldcloud_collaborators(
          app_data$token,
          qfieldcloud_url,
          project_id
        )

        print(collabs)

        # get project managers / admins
        manager_collabs <- collabs %>%
          dplyr::filter(role != "admin" | role != "manager")

        # number of project manager admins
        manager_admin_count <- nrow(manager_collabs)

        output$admin_count <- renderText({
          manager_admin_count
        })

        # number of project collaborators
        collabs_count <- nrow(collabs)

        output$collabs_count <- renderText({
          collabs_count
        })

        # get project information
        project_info <- qfieldcloudR::get_qfieldcloud_project(
          app_data$token,
          qfieldcloud_url,
          project_id
        )
      }
    )

    # get project files
    tryCatch(
      error = function(cnd) {
        showNotification("Could not load project files.", type = "error")
      },
      {
        # get project files
        app_data$project_files <- qfieldcloudR::get_qfieldcloud_files(
          app_data$token,
          qfieldcloud_url,
          project_id
        )

        output$project_files <- DT::renderDataTable({
          app_data$project_files
        })
      }
    )

    download_waiter$hide()
  })

  # Download project --------------------------------------------------------

  output$download_project <- downloadHandler(
    filename = function() {
      paste0("project_", input$manager_projects, ".zip")
    },
    content = function(file) {
      req(input$manager_projects)
      req(app_data$project_files)

      download_waiter$show()

      tmp_files <- tryCatch(
        error = function(cnd) {
          showNotification("Could not download project files.", type = "error")

          error_message <- "Could not download project files."
          file_conn <- file(paste0(tmp_dir <- tempdir(), "/error.txt"))
          writeLines(error_message, file_conn)
          close(file_conn)
          tmp_files <- paste0(tmp_dir <- tempdir(), "/error.txt")
        },
        {
          files_df <- app_data$project_files

          project_id <- app_data$manager_projects %>%
            dplyr::filter(name == input$manager_projects) %>%
            dplyr::select(id)

          project_id <- project_id$id

          tmp_files <- c()

          for (i in 1:nrow(files_df)) {
            f_tmp <- files_df[i, ]
            fname <- f_tmp$name

            get_fname <- qfieldcloudR::get_qfieldcloud_file(
              app_data$token,
              qfieldcloud_url,
              project_id,
              fname
            )

            fpath <- get_fname$tmp_file

            tmp_files <- c(tmp_files, fpath)
          }

          tmp_files
        }
      )

      zip(
        zipfile = file,
        files = tmp_files,
        flags = "-r9Xj"
      )

      download_waiter$hide()
    },
    contentType = "application/zip"
  )

  # Collaborator management -------------------------------------------------
  observeEvent(input$manager_projects, {
    req(input$manager_projects)

    tryCatch(
      error = function(cnd) {
        showNotification("Could not load project collaborators and QFieldCloud users.", type = "error")
      },
      {
        # get collaborators
        project_id <- app_data$manager_projects %>%
          dplyr::filter(name == input$manager_projects) %>%
          dplyr::select(id)

        project_id <- project_id$id

        project_name <- input$manager_projects

        output$project_name <- renderText({
          project_name
        })

        # get project collaborators
        collabs <- qfieldcloudR::get_qfieldcloud_collaborators(
          app_data$token,
          qfieldcloud_url,
          project_id
        )

        app_data$collaborators <- collabs

        updateSelectInput(
          session,
          "project_collaborators",
          choices = paste0(collabs$collaborator, " (", collabs$role, ")")
        )

        # get users
        users <- qfieldcloudR::get_qfieldcloud_users(
          app_data$token,
          qfieldcloud_url
        )

        # get users that are not collaborators
        potential_users <- users %>%
          dplyr::filter(
            !username %in% collabs$collaborator
          )

        app_data$potential_users <- potential_users

        updateSelectInput(
          session,
          "potential_users",
          choices = paste0(potential_users$username, " (", potential_users$full_name, ")")
        )
      }
    )
  })


  # Manage collaborators ----------------------------------------------------------------

  # allow only selected users or selected collaborators
  observeEvent(input$project_collaborators, {
    req(input$project_collaborators)
    
    updateSelectInput(
      session,
      "potential_users",
      selected = ""
    )
    
    updateSelectInput(
      session,
      "action_type",
      choices = c(
        "update",
        "delete"
      )
    )
    
  })
  
  observeEvent(input$potential_users, {
    req(input$potential_users)
    
    updateSelectInput(
      session,
      "project_collaborators",
      selected = ""
    )
    
    updateSelectInput(
      session,
      "action_type",
      choices = c(
        "add"
      )
    )
    
    
  })
  
  observeEvent(input$apply_action, {
    req(input$user_role)
    req(input$action_type)

    tryCatch(
      error = function(cnd) {
        showNotification("Error updating, deleting, or adding users. Check that users are selected.", type = "error")
      },
      {

        # get collaborators
        project_id <- app_data$manager_projects %>%
          dplyr::filter(name == input$manager_projects) %>%
          dplyr::select(id)
        
        project_id <- project_id$id
        
        if (length(input$potential_users) > 0 & input$action_type == "add") {
          
          
          for (i in input$potential_users) {
            
            # get username
            username <- stringr::str_split(i, " \\(")[[1]][1]

            if (input$action_type == "add") {
              # update user
              add_collaborator_status <- qfieldcloudR::add_qfieldcloud_collaborator(
                app_data$token,
                qfieldcloud_url,
                project_id,
                username,
                input$user_role
              )

              if ("success" %in% add_collaborator_status) {
                showNotification(paste0("successfully added ", username))
              } else {
                showNotification(paste0("failed to add ", username), type = error)
              }
            }
          }
          
        }

        if (length(input$project_collaborators) > 0 & input$action_type == "delete") {
          
          for (i in input$project_collaborators) {
            
            # get username
            username <- stringr::str_split(i, " \\(")[[1]][1]
            
            # delete collaborator
            delete_collaborator_status <- qfieldcloudR::delete_qfieldcloud_collaborator(
              app_data$token,
              qfieldcloud_url,
              project_id,
              username
            )

            if ("success" %in% delete_collaborator_status) {
              showNotification(paste0("successfully deleted ", username))
            } else {
              showNotification(paste0("failed to delete ", username), type = error)
            }
          }

        }

        if (length(input$project_collaborators) > 0 & input$action_type == "update") {
          
          for (i in input$project_collaborators) {

            # get username
            username <- stringr::str_split(i, " \\(")[[1]][1]
            
            # update collaborator
            update_collaborator_status <- qfieldcloudR::update_qfieldcloud_collaborator(
              app_data$token,
              qfieldcloud_url,
              project_id,
              username,
              input$user_role
            )

            if ("success" %in% update_collaborator_status) {
              showNotification(paste0("successfully updated ", username))
            } else {
              showNotification(paste0("failed to update ", username), type = error)
            }
          }

        }
        
        # now update selectInputs for collaborators and users
        updating_user_waiter$show()

        # get project collaborators
        collabs <- qfieldcloudR::get_qfieldcloud_collaborators(
          app_data$token,
          qfieldcloud_url,
          project_id
        )

        app_data$collaborators <- collabs

        updateSelectInput(
          session,
          "project_collaborators",
          choices = paste0(collabs$collaborator, " (", collabs$role, ")")
        )

        # get users
        users <- qfieldcloudR::get_qfieldcloud_users(
          app_data$token,
          qfieldcloud_url
        )

        # get users that are not collaborators
        potential_users <- users %>%
          dplyr::filter(
            !username %in% collabs$collaborator
          )

        app_data$potential_users <- potential_users

        updateSelectInput(
          session,
          "potential_users",
          choices = paste0(potential_users$username, " (", potential_users$full_name, ")")
        )

        updating_user_waiter$hide()
      }
    )
  })
})
