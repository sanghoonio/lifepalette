library(shiny)
library(shinyjs)
library(bslib)
library(tidyverse)

ui <- bootstrapPage(
  
  useShinyjs(),
  theme = bs_theme(version = 5),
  
  tags$head(
    tags$title('LifePalette'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
    tags$link(rel = 'icon', type = 'image/x-icon', href = '/favicon.ico'),
    tags$link(rel = 'apple-touch-icon', sizes='180x180', href = '/apple-touch-icon.png'),
    tags$link(rel = 'manifest', href = '/site.webmanifest'),
    tags$script(src = 'script.js'),
  ),
  
  tags$meta(name = 'theme-color', content = '#ddd'),
  
  div(
    class = 'container-fluid',
    div(
      class = 'row',
      
      ## dropdown ----
      div(
        style = 'height:48px;',
        shinyjs::disabled(tags$button(
          class = 'btn btn_settings',
          style = 'float:right; padding:6px 12px; margin:5px 5px 0 0; border:none;',
          span(id = 'signin_text', 'Sign In')
        )
        ))
    ),
    
    div(
      class = 'row',
      style = 'background-color:#fff; padding-top:48px;',
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
              div(
                id = 'dob_inputs',
                tags$select(
                  class = 'dob_input',
                  id = 'year_input',
                  style = 'width:5ch;',
                ),
                tags$select(
                  class = 'dob_input',
                  id = 'month_input',
                  style = 'width:9ch;',
                ),
                tags$select(
                  class = 'dob_input',
                  id = 'day_input',
                  style = 'width:3ch;',
                ),
                tags$button(
                  id = 'submit_dob',
                  'Go!'
                )
              )
            ),
            
            div(
              id = 'boxes_div',
              style = 'padding-top:40px;',
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
            
            p(
              id = 'contact',
              style = 'padding-top:30px; margin:0; font-size: 1.5em;',
              a(
                href = 'https://github.com/sanghoonio/lifepalette',
                icon(name = 'square-github')
              ),
              a(
                href = 'https://www.linkedin.com/in/sanghoonio/',
                icon(name = 'linkedin')
              ),
            ),
            p(
              id = 'credits',
              style = 'font-size:12px;',
              'Inspired by ', a('Your Life in Weeks', href = 'https://waitbutwhy.com/2014/05/life-weeks.html'), ' by Tim Urban.'
            )
          )
        )
      )
    )
  )
)
