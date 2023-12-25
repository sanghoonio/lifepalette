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
  extendShinyjs(script = 'script.js', functions = c('fillBoxes', 'fillBox')),
  
  tags$head(
    tags$title('LifePalette'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')
  ),
  
  div(
    class = 'row',
    ## dropdown ----
    div(
      style = 'height:48px;',
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
    style = 'background-color:#fff; padding-top:40px;',
    div(
      class = 'col-12',
      
      ## body ----
      div(
        class = 'row',
        div(
          class = 'col-12',
          style = 'text-align:center;',
          h3(id = 'page_title', 'LifePalette'),
          
          ### birthdate input ----
          div(
            id = 'dob_div',
            style = 'padding-top:40px;',
            p(style = 'margin:0 0 10px 0;', 'Enter your birthdate.'),
            dateInput(inputId = 'dob', label = NULL, value = Sys.Date() %>% as.character(), startview = 'decade', width = '100%'),
          ),
          
          div(
            id = 'boxes_div',
            div(
              class = 'x_label wide_label',
              span('years', HTML('&nbsp;'), icon('arrow-right-long')),
            ),
            div(
              class = 'x_label tall_label',
              span('weeks', HTML('&nbsp;'), icon('arrow-right-long')),
            ),
            
            ### boxes ----
            div(
              class = 'grid_parent',
              
              div(
                class = 'y_label wide_label',
                style = 'margin:0 6px 0 0;',
                span(icon('arrow-up-long'), HTML('&nbsp;'), 'weeks'),
              ),
              div(
                class = 'y_label tall_label',
                style = 'margin:0 6px 0 0;',
                span(icon('arrow-up-long'), HTML('&nbsp;'), 'years'),
              ),
              
              div(
                id = 'grid_container',
                lapply(1:92, function(year) {
                  lapply(1:53, function(week) {
                    current_box <- week + (year-1)*53
                    current_week <- week-1 + (year-2)*52

                    if (year == 1 & week == 1) {
                      div(class = 'grid_none', id = 'label_0')
                    } else if (year == 1 & week != 1) {
                      div(class = 'grid_label_week', id = paste0('label_week_', week-1), p(week-1))
                    } else if (year != 1 & week == 1) {
                      div(class = 'grid_label_year', id = paste0('label_year_', year-1), p(year-2))
                    } else {
                      div(class = 'grid_item', id = paste0('week_', sprintf('%04d', current_week)))
                    }
                  })
                })
              ),
              
              div(
                class = 'y_label',
                style = 'visibility:hidden; margin:0 0 0 6px;',
                span(icon('arrow-up-long')),
              )
            )
          ),
          
          p(style = 'font-size:12px; padding-top:30px;',
            'Inspired by ', a('Your Life in Weeks', href = 'https://waitbutwhy.com/2014/05/life-weeks.html'), ' by Tim Urban.'
          )
        )
      )
    )
  )
)
