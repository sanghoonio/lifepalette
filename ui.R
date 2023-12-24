library(shiny)
library(shinyjs)
library(bslib)
library(tidyverse)

library(firebase)

ui <- fluidPage(
  
  useShinyjs(),
  useFirebase(),
  theme = bs_theme(version = 5),
  
  tags$script(src = 'script.js'),
  
  tags$head(
    tags$title('LifePalette'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')
  ),
  
  div(
    class = 'row',
    ## dropdown ----
    div(
      style = 'height:46px;',
      tags$button(
        class = 'btn',
        style = 'float:right; padding:6px 12px; margin:5px 5px 0 0;',
        `data-bs-toggle` = 'dropdown',
        `aria-expanded` = 'false',
        icon('bars')
      ),
      
      tags$ul(
        class = 'dropdown-menu custom_dropdown',
        style = 'width:275px;',
        `aria-labeled-by` = 'settings_dropdown',
        
        uiOutput(outputId = 'user'),
        div(id = 'login_div')
      )
    )
  ),
  
  div(
    class = 'row',
    style = 'background-color:#fff;',
    div(
      class = 'col-12',
      
      ## body ----
      div(
        class = 'row',
        div(
          class = 'col-12',
          style = 'text-align:center;',
          br(),
          h3('LifePalette'),
          br(),
          br(),
          
          ### birthdate input ----
          div(
            id = 'dob_div',
            p(style = 'margin:0 0 10px 0;', 'Enter your birthdate.'),
            dateInput(inputId = 'dob', label = NULL, value = Sys.Date() %>% as.character(), startview = 'decade', width = '100%'),
          ),
          
          br(),
          br(),
          
          div(
            id = 'x_label',
            style = 'margin:0 0 6px 0; padding:0; font-size:11px;',
            span('years', HTML('&nbsp;'), icon('arrow-right-long')),
          ),
          
          ### boxes ----
          div(
            class = 'grid_parent',
            
            div(
              id = 'y_label',
              style = 'writing-mode:vertical-rl; transform:scale(-1); margin:0 6px 0 0; padding:0; font-size:11px;',
              span(icon('arrow-up-long'), HTML('&nbsp;'), 'weeks'),
            ),
            
            uiOutput(outputId = 'grid_container'),
            
            div(
              id = 'y_label',
              style = 'writing-mode:vertical-rl; transform:scale(-1); margin:0 0 0 6px; padding:0; font-size:11px; visibility:hidden;',
              span(icon('arrow-up-long'), HTML('&nbsp;'), 'weeks'),
            ),
            
          ),
          
          br(),
          br(),
          
          p(style = 'font-size:12px;',
            'Inspired by ', a('Your Life in Weeks', href = 'https://waitbutwhy.com/2014/05/life-weeks.html'), ' by Tim Urban.'
          )
        )
      )
    )
  )
)
