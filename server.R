library(shiny)
library(shinyjs)
library(tidyverse)

library(DBI)

server <- function(input, output, session) {
  
  ## auth ----
  f <- FirebaseEmailPassword$new(config_path = 'data/firebase.rds', 
                                 persistence = 'session'
  )
  
  # open modals
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
    
    if (created$success){
      removeModal()
      showNotification('Account created!', type = 'message')
      
      print(created$response$user$email)
      
      temp_data <- c(created$response$user$email, input$dob %>% as.character())
      add_data(new_data = list(temp_data, 1:length(temp_data)), new_col = created$response$user$email, db_table = 'user_data')
      
      temp_colors <- default_colors()[, 1]
      color_diff <- which(temp_colors != 'black')
      add_data(new_data = list(temp_colors[color_diff], color_diff), new_col = created$response$user$email, db_table = 'user_colors')
      
      temp_comments <- default_comments()[, 1]
      comment_diff <- which(temp_comments != '')
      add_data(new_data = list(temp_comments[comment_diff], comment_diff), new_col = created$response$user$email, db_table = 'user_comments')
      
    } else {
      showNotification('Error!', type = 'error')
    }
  })
  
  ## sign in/out ----
  observeEvent(input$sign_in, {
    removeModal()
    f$sign_in(input$email_signin, input$password_signin)
  })
  
  observeEvent(input$sign_out, {
    f$sign_out()
  })
  
  ### current user ----
  current_user <- reactiveVal(value = 'default')
  
  #### set current user reactiveval ----
  observe({
    if (!is.null(f$get_signed_in())) {
      current_user(f$get_signed_in()$response$email)
    } else {
      current_user('default')
    }
  })
  
  #### do when current user reactiveval updates ----
  observeEvent(current_user(), {
    if (current_user() != 'default') {
      user_dob <- get_data(tbl_column = current_user(), db_table = 'user_data')[2, 1]
      updateDateInput(inputId = 'dob', value = user_dob)
      
      removeUI(selector = '#register_modal')
      removeUI(selector = '#signin_modal')
      insertUI(selector = '#login_div',
               where = 'beforeEnd',
               actionButton(style = 'float:right; width:100%;', class = 'btn btn-light', 'sign_out', 'Sign Out')
      )
    } else {
      updateDateInput(inputId = 'dob', value = Sys.Date() %>% as.character())
      
      insertUI(selector = '#login_div',
               where = 'beforeEnd',
               actionButton(style = 'float:right;', class = 'btn btn-light', style = 'margin-left:10px;', 'register_modal', 'Register')
      )
      insertUI(selector = '#login_div',
               where = 'beforeEnd',
               actionButton(style = 'float:right;', class = 'btn btn-light', 'signin_modal', 'Sign In')
      )
      removeUI(selector = '#sign_out')
    }
  })
  
  output$user <- renderUI({
    user <- current_user()
    if (user != 'default') {
      # paste0('Logged in: ', user)
      div(
        style = 'text-align:center; clear:both; margin:10px 0 30px 0;',
        p(user)
      )
    } else {
      NULL
    }
  })
  
  ## weeks elapsed from dob ----
  weeks_diff <- reactiveVal(0)
  observeEvent(input$dob, {
    weeks_diff((difftime(Sys.Date(), input$dob, units = 'weeks') * .9972594543) %>% floor())
    
    if (current_user() != 'default') {
      update_data(value = input$dob, 
                  index = 2, 
                  tbl_column = current_user(), 
                  db_table = 'user_data'
      )
    }
  })
  
  ## draw boxes ----
  force_update <- reactiveVal(0)
  
  boxes <- list()
  boxes_processed <- reactive({
    force_update()
    
    if (current_user() != 'default') {
      user_colors <- get_data(tbl_column = current_user(), db_table = 'user_colors')
    } else {
      user_colors <- default_colors()
    }
    
    # browser()
    print(weeks_diff())
    
    boxes <- lapply(1:92, function(year) {
      lapply(1:53, function(week) {
        current_box <- week + (year-1)*53
        current_week <- week-1 + (year-2)*52
        if (year == 1 & week == 1) {
          boxes[[current_box]] <- div(
            class = 'grid_none',
            id = paste0('box_', sprintf('%04d', current_box), '_label_0'),
          )
        } else if (year == 1 & week != 1) {
          boxes[[current_box]] <- div(
            class = 'grid_label_week',
            id = paste0('box_', sprintf('%04d', current_box), '_label_week_', week-1),
            p(week-1)
          )
        } else if (year != 1 & week == 1) {
          boxes[[current_box]] <- div(
            class = 'grid_label_year',
            id = paste0('box_', sprintf('%04d', current_box), '_label_year_', year-1),
            p(year-2)
          )
        } else {
          if (current_week <= weeks_diff()) {
            boxes[[current_box]] <- a(
              class = paste0('grid_item filled ', user_colors[current_week, 1]),
              id = paste0('box_', sprintf('%04d', current_box), '_week_', sprintf('%04d', current_week))
            )
          } else {
            boxes[[current_box]] <- div(
              class = 'grid_item',
              id = paste0('box_', sprintf('%04d', current_box), '_week_', sprintf('%04d', current_week))
            )
          }
        }
        
      })
    })
  })
  
  ## output boxes ----
  output$grid_container <- renderUI(boxes_processed())
  
  ## db functions ----
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
  
  ## update db data ----
  
  ## modal ----
  observeEvent(input$click_filled, {
    user_colors <- get_data(tbl_column = current_user(), db_table = 'user_colors')
    user_comments <- get_data(tbl_column = current_user(), db_table = 'user_comments')
    
    selected_id <- input$click_filled
    selected_week <- selected_id %>% substr(nchar(selected_id)-3, nchar(selected_id)) %>% as.numeric()
    
    print(selected_id)
    
    showModal(
      modalDialog(
        title = paste0('Week ', selected_week, '...'),
        textInput(inputId = 'week_number', label = 'week', value = selected_week) %>% hidden(),
        # selectInput(inputId = 'set_color',
        #             label = 'Color',
        #             width = '100%',
        #             choices = c('black', 'red', 'orange', 'yellow', 'green', 'blue', 'purple', 'brown'),
        #             selected = user_colors[selected_week, 1]
        # ),
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
    } else {
      temp_colors <- default_colors()
      temp_colors[selected_week, 1] <- input$set_color
      default_colors(temp_colors)
      
      temp_comments <- default_comments()
      temp_colors[selected_week, 1] <- input$set_comment
      default_comments(temp_comments)
    }
    
    force_update(force_update() + 1)
  })
  
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
  
}
