library(shiny)
library(shinyjs)
library(tidyverse)

library(DBI)

server <- function(input, output, session) {
  
  ## helper functions ----
  get_data <- function(tbl_column, db_table) {
    con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')
    got_data <- dbGetQuery(con, paste0('SELECT "', tbl_column, '" FROM ', db_table))
    dbDisconnect(con)
    
    return(got_data)
  }
  
  update_data <- function(value, index, tbl_column, db_table) {
    con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')
    query <- paste0('UPDATE ', db_table, ' SET "', tbl_column, '"="', value, '" WHERE "index"=', index, ';')
    dbExecute(con, query)
    dbDisconnect(con)
    
    return(0)
  }
  
  add_data <- function(new_data, new_col, db_table) {
    con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')
    query <- paste0('ALTER TABLE "', db_table, '" ADD COLUMN "', new_col, '" TEXT;')
    dbExecute(con, query)
    query <- paste0('UPDATE "', db_table, '" SET "', new_col, '" = "default";')
    dbExecute(con, query)
    query <- paste0('UPDATE "', db_table, '" SET "', new_col, '" = ? WHERE "index" = ?;')
    dbExecute(con, query, params = new_data)
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
  user_colors <- reactiveVal(
    data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
      default = rep('black', 52*91),
    )
  )
  
  default_colors <- reactiveVal(
    data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
      default = rep('black', 52*91),
    )
  )
  
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
      
      temp_data <- c(user_email, input$dob %>% as.character())
      add_data(new_data = list(temp_data, 1:length(temp_data)), new_col = user_email, db_table = 'user_data')
      
      temp_colors <- default_colors()[, 1]
      color_diff <- which(temp_colors != 'black')
      add_data(new_data = list(temp_colors[color_diff], color_diff), new_col = user_email, db_table = 'user_colors')
      
      temp_comments <- default_comments()[, 1]
      comment_diff <- which(temp_comments != '')
      add_data(new_data = list(temp_comments[comment_diff], comment_diff), new_col = user_email, db_table = 'user_comments')
      
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
    
    user_colors(default_colors())
    
    freezeReactiveValue(input, 'dob')
    updateDateInput(inputId = 'dob', value = Sys.Date() %>% as.character())
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
    
    user_colors(get_data(tbl_column = user, db_table = 'user_colors'))
    
    freezeReactiveValue(input, 'dob')
    user_dob <- get_data(tbl_column = user, db_table = 'user_data')[2, 1]
    updateDateInput(inputId = 'dob', value = user_dob)
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
  
  ## calc weeks/boxes ----
  observeEvent(input$dob, {
    dob <- input$dob
    
    if (current_user() != 'default') {
      update_data(value = dob, 
                  index = 2, 
                  tbl_column = current_user(), 
                  db_table = 'user_data'
      )
    }
    
    weeks_diff <- ((difftime(Sys.Date(), dob, units = 'weeks') * .9972594543) %>% floor())
    box_classes <- lapply(1:(52*90), function(week) {
      ifelse((week < weeks_diff), paste0('grid_item filled ', user_colors()[week, 1]), 'grid_item')
    })
    js$fillBoxes(box_classes)
  }, ignoreInit = TRUE)

  ### show modal ----
  observeEvent(input$click_filled, {
    user_colors <- get_data(tbl_column = current_user(), db_table = 'user_colors')
    user_comments <- get_data(tbl_column = current_user(), db_table = 'user_comments')
    
    selected_id <- input$click_filled
    selected_week <- selected_id %>% substr(nchar(selected_id)-3, nchar(selected_id)) %>% as.numeric()
    
    showModal(
      modalDialog(
        title = paste0('Week ', selected_week, '...'),
        textInput(inputId = 'week_number', label = 'week', value = selected_week) %>% hidden(),
        p(style = 'margin-bottom: 8px;', 'Color'),
        div(
          style = 'margin-left:56px;',
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
    
    selected_week <- input$week_number %>% as.numeric()
    
    if (current_user() != 'default') {
      update_data(value = input$set_color, 
                  index = selected_week, 
                  tbl_column = current_user(), 
                  db_table = 'user_colors'
      )
      update_data(value = input$set_comment, 
                  index = selected_week, 
                  tbl_column = current_user(), 
                  db_table = 'user_comments'
      )
      user_colors(get_data(tbl_column = current_user(), db_table = 'user_colors'))
    } else {
      temp_colors <- default_colors()
      temp_colors[selected_week, 1] <- input$set_color
      default_colors(temp_colors)
      user_colors(temp_colors)
      
      temp_comments <- default_comments()
      temp_colors[selected_week, 1] <- input$set_comment
      default_comments(temp_comments)
    }
    
    box_class <- paste0('grid_item filled ', user_colors()[selected_week, 1])
    js$fillBox(box_class, selected_week)
  })
  
}
