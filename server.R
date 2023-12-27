library(shiny)
library(shinyjs)
library(tidyverse)

library(DBI)

server <- function(input, output, session) {
  
  ## helper functions ----
  get_data <- function(where, index_column, tbl_column, db_table) {
    con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')
    got_data <- dbGetQuery(con, paste0('SELECT "', tbl_column, '" FROM "', db_table, '" WHERE "', index_column, '"="', where, '";'))
    dbDisconnect(con)
    
    return(got_data)
  }
  
  update_data <- function(value, user, where, index_column, tbl_column, db_table) {
    con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')
    query <- paste0('UPDATE ', db_table, ' SET "', tbl_column, '"="', value, '" WHERE "', index_column, '"="', where, '" AND', '"user"="', user, '";')
    dbExecute(con, query)
    dbDisconnect(con)
    
    return(0)
  }
  
  append_data <- function(new_data, db_table) {
    con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')
    dbWriteTable(con, db_table, new_data, append = TRUE)
    dbDisconnect(con)
    
    return(0)
  }
  
  ## store data ----
  ### used for rendering colors
  render_colors <- reactiveVal(
    data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
      default = rep('black', 52*91),
    )
  )
  
  ### used for keeping temp data for default and newly created user
  default_colors <- reactiveVal(
    data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
      default = rep('black', 52*91),
    )
  )
  
  ### used for keeping temp data for default and newly created user
  default_comments <- reactiveVal(
    data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
      default = rep('', 52*91),
    )
  )
  
  ## auth ----
  f <- FirebaseEmailPassword$new(config_path = 'data/firebase.rds')
  
  observeEvent(input$register_modal, {
    showModal(
      modalDialog(
        title = 'Register',
        textInput('email_create', 'Your email', width = '100%'),
        passwordInput('password_create', 'Your password', width = '100%'),
        footer = tagList(
          modalButton('Cancel'),
          actionButton('create', 'Register')
        ),
        easyClose = FALSE
      ))
  })
  
  observeEvent(input$signin_modal, {
    showModal(
      modalDialog(
        title = 'Sign in',
        textInput('email_signin', 'Your email', width = '100%'),
        passwordInput('password_signin', 'Your password', width = '100%'),
        footer = tagList(
          modalButton('Cancel'),
          actionButton('sign_in', 'Sign in')
        ),
        easyClose = FALSE
      ))
  })
  
  ## create user ----
  observeEvent(input$create, {
    f$create(input$email_create, input$password_create)
  })
  
  observeEvent(f$get_created(), {
    created <- f$get_created()
    
    if (created$success) {
      removeModal()
      showNotification('Account created!', type = 'message')
      
      user_email <- created$response$user$email
      
      ### use current default user data as new user data
      user_list <- data.frame(matrix(nrow = 1, ncol = 0)) %>% mutate(
        user = user_email,
        dob = input$dob %>% as.character()
      )
      
      user_data <- data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
        week = 1:(52*91),
        user = rep(user_email, 52*91),
        color = default_colors()[, 1],
        comment = default_comments()[, 1]
      )
      
      append_data(new_data = user_list, db_table = 'user_list')
      append_data(new_data = user_data, db_table = 'user_data')
      
    } else {
      if (created$response$code == 'auth/email-already-in-use') {
        error_msg <- 'Error: email already in use.'
      } else if (created$response$code == 'auth/weak-password') {
        error_msg <- 'Error: password is too weak.'
      } else {
        error_msg <- 'Error: account could not be made.'
      }
      showNotification(error_msg, type = 'error')
    }
  })
  
  ## sign in/out ----
  observeEvent(input$sign_in, {
    f$sign_in(input$email_signin, input$password_signin)
    sign_in_clicked(1)
  })
  
  observeEvent(input$sign_out, {
    f$sign_out()
    current_user('default')
    
    render_colors(default_colors())
    
    today <- Sys.Date()
    freezeReactiveValue(input, 'dob')
    js$updateDOBInputs(as.numeric(format(today, '%Y')), as.numeric(format(today, '%m')), as.numeric(format(today, '%d')))
  })
  
  ### current user ----
  current_user <- reactiveVal(value = 'default')
  
  sign_in_clicked <- reactiveVal(0)

  #### set current user reactiveval ----
  observeEvent(f$get_signed_in(), {
    user <- f$get_signed_in()$response$email
    current_user(user)
    
    if (sign_in_clicked() == 1) {
      removeModal()
      showNotification('Login successful!', type = 'message')
      sign_in_clicked(0)
    }
    
    render_colors(get_data(where = user, index_column = 'user', tbl_column = 'color', db_table = 'user_data'))
    
    user_dob <- get_data(where = user, index_column = 'user', tbl_column = 'dob', db_table = 'user_list')[1, 1] %>% as.Date(format = '%Y-%m-%d')
    freezeReactiveValue(input, 'dob')
    js$updateDOBInputs(as.numeric(format(user_dob, '%Y')), as.numeric(format(user_dob, '%m')), as.numeric(format(user_dob, '%d')))
  })
  
  #### do when current user reactiveval updates ----
  observeEvent(current_user(), {
    if (current_user() != 'default') {
      removeUI(selector = '#register_modal')
      removeUI(selector = '#signin_modal')
      insertUI(selector = '#login_div',
               where = 'beforeEnd',
               actionButton(style = 'width:100%;', class = 'btn btn-light', 'sign_out', 'Sign Out')
      )
    } else {
      insertUI(selector = '#login_div',
               where = 'beforeEnd',
               actionButton(style = 'margin-right:10px; width:calc(50% - 5px);', class = 'btn btn-light', 'register_modal', 'Register')
      )
      insertUI(selector = '#login_div',
               where = 'beforeEnd',
               actionButton(style = 'width:calc(50% - 5px); ', class = 'btn btn-light', 'signin_modal', 'Sign In')
      )
      removeUI(selector = '#sign_out')
    }
  })
  
  output$user <- renderUI({
    user <- current_user()
    if (user != 'default') {
      div(
        style = 'text-align:center; clear:both; margin:10px 0 30px 0;',
        p(user)
      )
    } else {
      NULL
    }
  })
  
  ## calc weeks/boxes with date input ----
  observeEvent(input$dob, {
    dob <- input$dob
    
    if (current_user() != 'default') {
      update_data(value = dob,
                  user = current_user(),
                  where = current_user(), 
                  index_col = 'user',
                  tbl_column = 'dob', 
                  db_table = 'user_list'
      )
    }
    
    weeks_diff <- ((difftime(Sys.Date(), dob, units = 'weeks') * 52*7/365.25) %>% floor())
    box_classes <- lapply(1:(52*91), function(week) {
      ifelse((week <= weeks_diff), paste0('grid_item filled ', render_colors()[week, 1]), 'grid_item')
    })
    js$fillBoxes(box_classes)
  }, ignoreInit = TRUE)

  ### show modal ----
  observeEvent(input$click_filled, {
    user <- current_user()
    
    if (user != 'default') {
      user_colors <- get_data(where = user, index_column = 'user', tbl_column = 'color', db_table = 'user_data')
      user_comments <- get_data(where = user, index_column = 'user', tbl_column = 'comment', db_table = 'user_data')
    } else {
      user_colors <- default_colors()
      user_comments <- default_comments()
    }
    
    selected_id <- input$click_filled
    selected_week <- selected_id %>% substr(nchar(selected_id)-3, nchar(selected_id)) %>% as.numeric()
    
    showModal(
      modalDialog(
        title = paste0('Week ', selected_week, '...'),
        textInput(inputId = 'week_number', label = 'week', value = selected_week) %>% hidden(),
        p(style = 'margin-bottom: 8px;', 'Color'),
        div(
          radioButtons(inputId = 'set_color',
                       label = NULL, 
                       inline = TRUE,
                       selected = user_colors[selected_week, 1],
                       choiceValues = c('black', 'red', 'orange', 'yellow', 'green', 'blue', 'purple', 'brown'),
                       choiceNames = c('','','','','','','','')
          ),
        ),
        
        br(),
        textAreaInput(inputId = 'set_comment',
                      label = 'Comment',
                      width = '100%',
                      value = user_comments[selected_week, 1],
                      placeholder = 'type your comment here...'
        ),
        footer = tagList(
          modalButton('Cancel'),
          actionButton(inputId = 'close_modal', label = 'Confirm')
        ),
        easyClose = FALSE
      )
    )
  })
  
  ### close modal ----
  observeEvent(input$close_modal, {
    removeModal()
    
    user <- current_user()
    selected_week <- input$week_number %>% as.numeric()
    
    if (current_user() != 'default') {
      update_data(value = input$set_color,
                  user = user,
                  where = selected_week,
                  index_column = 'week',
                  tbl_column = 'color', 
                  db_table = 'user_data'
      )
      update_data(value = input$set_comment,
                  user = user,
                  where = selected_week,
                  index_column = 'week',
                  tbl_column = 'comment', 
                  db_table = 'user_data'
      )
      render_colors(get_data(where = user, index_column = 'user', tbl_column = 'color', db_table = 'user_data'))
    } else {
      temp_colors <- default_colors()
      temp_colors[selected_week, 1] <- input$set_color
      default_colors(temp_colors)
      render_colors(temp_colors)
      
      temp_comments <- default_comments()
      temp_comments[selected_week, 1] <- input$set_comment
      default_comments(temp_comments)
    }
    
    box_class <- paste0('grid_item filled ', render_colors()[selected_week, 1])
    js$fillBox(box_class, selected_week)
  })
  
}
