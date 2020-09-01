library(shiny)
library(shinyWidgets)
library(tidyverse)
library(scrollytell)
library(extrafont)
library(plotly)
library(spotifyr)
library(tidyverse)
library(highcharter)
library(ggbeeswarm)
library(plotly)
library(sentimentr)
library(magrittr)
library(dplyr)

source('utils.R')

ui <- fluidPage(
    title = "The Birth, Death, and Rebirth of Kanye West",
    theme = 'style.css', 
    
    # Import Google font
    HTML("<style>
@import url('https://fonts.googleapis.com/css?family=Lato&display=swap');
@import url('https://fonts.googleapis.com/css2?family=Adobe-Caslon&display=swap');
@import url('https://use.typekit.net/ful5wqp.css');
</style>"),
    
    # Code to suppress warning messages while data is loading on-screen 
    # reference (https://groups.google.com/forum/#!topic/shiny-discuss/FyMGa2R_Mgs)
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    # Parallax introductory image
    fluidRow(
        # Parallax reference: https://www.w3schools.com/howto/howto_css_parallax.asp
        HTML('<div class="parallax" id="section-one"></div>')
    ),
    
    # Article title
    fluidRow(HTML("<center>
                <h1>The Birth, Death, and Rebirth of Kanye West</h1>
                <p style='size:18px';> by <a href='https://connorrothschild.com/' target='_blank'>Connor Rothschild</a></p>
                </center>")
    ),
    br(),
    
    fluidRow(
        column(1),
        column(10,
               # Introduction
               fluidRow(id='text',
                        column(1),
                        column(10, 
                               br(),
                               HTML(
                        "<h2>Introduction.</h2>
                               Introductory paragraph.
                        <br><br>
                               Introductory paragraph.
                        <br><br>
                               Introductory paragraph.
                                    </p>"),
                               br()
                        ),
                        column(1)
               ),
               
               br(), 
               br(),
               
               # Scrolly container
               scrolly_container("type",
                                 scrolly_graph(br(), 
                                               br(),
                                               ## Only display if not intro
                                               conditionalPanel(condition = 'input.type != "intro"'
                                                                # div(uiOutput('metric_list'), style = 'font-size: 80%')
                                                                ),
                                               HTML('<center>'),
                                               highchartOutput('metrics', height = '600px'),
                                               HTML('</center>')
                                 ),
                                 
                                 scrolly_sections(
                                     scrolly_section(id = 'intro',
                                                     HTML('<h2>Subheading</h2>'),
                                                     HTML("<p>Lorem ipsum dolor sit amet, et malesuada conubia id, sapien et. Risus erat sagittis, nunc neque, scelerisque vel. Eu suspendisse lorem nullam eu risus feugiat ac lobortis eros? Dui ut mi nibh mauris urna neque sed, enim tempus tincidunt nunc ante eros ullamcorper et. In ipsum praesent vel faucibus tempus in congue ipsum lacinia. Facilisis volutpat vitae, dis, sagittis aenean nec ut. Pretium quis tincidunt rhoncus auctor enim erat nisi taciti ut donec eget. Quis, vehicula suscipit non ac netus nam facilisi. Facilisis inceptos velit dolor et fringilla luctus cras. Gravida nisl ut eget fusce fusce? At risus mauris urna non sodales ornare urna venenatis. Inceptos sociis tincidunt laoreet vitae. Id augue ut dignissim erat mi. Cras velit class vel ultrices ligula lacinia.</p>")
                                     ),
                                     HTML('<center>'),
                                     scrolly_section(id = "suv",
                                                     h2("SUVs"),
                                                     p("Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum")
                                     ),
                                     scrolly_section(id = "subcompact",
                                                     h2("Subcompact"),
                                                     p("Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum")
                                     ),
                                     scrolly_section(id = "compact",
                                                     h2("Compact"),
                                                     p("Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum Lorem ipsum")
                                     ),
                                     HTML('</center>'),
                                     scrolly_section(id = "all",
                                                     HTML('<p>During the <b><font color="#002a54">1991-92 season</font></b>, which served as a warm-up to the Olympic games, two players did not play professionally: Christian Laettner, who was at Duke, and Magic Johnson, who suddenly retired. With the exception of an injury-plagued Larry Bird and young David Robinson, the rest of the team was very much in the primes of their careers.</p>'),
                                                     br(), 
                                                     br(),
                                                     br()
                                     ),
                                     scrolly_section(id = "type_blank",
                                                     br()
                                     )
                                 ) # close scrolly_sections
               ) # close scrolly_Container
        ),
        column(1)), #close section 1 fluidRow
   
    ## BEGIN SECTION TWO
    # fluidRow(HTML('<div class="parallax" id="section-two"></div>')), 
    # 
    # fluidRow(
    #     column(1),
    #     column(10,
    #            # Introduction
    #            fluidRow(id='text',
    #                     column(2),
    #                     column(8, 
    #                            br(),
    #                            HTML(
    #                                "<p><span style='font-size:30px'><b>Introduction</b></span>.
    #                            Introductory paragraph.
    #                     <br><br>
    #                            Introductory paragraph.
    #                     <br><br>
    #                            Introductory paragraph.
    #                                 </p>"),
    #                            br()
    #                     ),
    #                     column(2)
    #            ),
    #            
    #            br(), 
    #            br(),
    #            
    #            # Scrolly container
    #            scrolly_container("type",
    #                              scrolly_graph(br(), 
    #                                            br(),
    #                                            conditionalPanel(condition = 'input.type != "intro"',
    #                                                             div(uiOutput('metric_list'), style = 'font-size: 80%')),
    #                                            HTML('<center>'),
    #                                            # highchartOutput('metrics', height = '560px'),
    #                                            HTML('</center>')
    #                              ),
    #                              
    #                              scrolly_sections(
    #                                  scrolly_section(id = 'section-two-intro',
    #                                                  HTML('<h2>Subheading</h2>'),
    #                                                  HTML("<p>If ever a team existed that required no introduction, the 1992 Dream Team is it. The type was stacked with 11-of-12 future hall of famers and served as a who's who of NBA elite. Below is a look at the players and their career stats (the age and NBA team listed are as of the 1992 Olympics).</p>")
    #                                  ),
    #                                  HTML('<center>'),
    #                                  scrolly_section(id = "Jordan2",
    #                                                  tags$img(src = 'jordan.png'),
    #                                                  HTML(team_data$Label[team_data$Name == 'Michael Jordan'])
    #                                  ),
    #                                  scrolly_section(id = "Magic2",
    #                                                  tags$img(src = 'magic.png'),
    #                                                  HTML(team_data$Label[team_data$Name == 'Magic Johnson'])
    #                                  ),
    #                                  scrolly_section(id = "Bird2",
    #                                                  tags$img(src = 'bird.png'),
    #                                                  HTML(team_data$Label[team_data$Name == 'Larry Bird'])
    #                                  ),
    #                                  HTML('</center>'),
    #                                  scrolly_section(id = "1992-2",
    #                                                  HTML('<p>During the <b><font color="#002a54">1991-92 season</font></b>, which served as a warm-up to the Olympic games, two players did not play professionally: Christian Laettner, who was at Duke, and Magic Johnson, who suddenly retired. With the exception of an injury-plagued Larry Bird and young David Robinson, the rest of the team was very much in the primes of their careers.</p>'),
    #                                                  br(), 
    #                                                  br(),
    #                                                  br()
    #                                  ),
    #                                  scrolly_section(id = "type_blank-2",
    #                                                  br()
    #                                  )
    #                              ) # close scrolly_sections
    #            ) # close scrolly_Container
    #     ),
    #     column(1)), #close section 2 fluidRow            
    
    # BEGIN CONCLUSION
               
    fluidRow( HTML('<div class="parallax" id="section-two"></div>')), 
               
    fluidRow(
        column(1),
        column(10,
               div(fluidRow(id = 'text',
                            column(1),
                            column(10, 
                                   br(),
                                   HTML("<h2>Death.</h2> 
                                   
                                As expected, once the men's Olympic basketball competition began, there was only one team that could leave Barcelona with the gold medal.  The United States cruised through all eight of its contests. In fact, during several games, it appeared as though the opposing team was just happy to be on the same floor as the NBA players.<br><br>
                                
                                Team USA averaged 117 points per game and held opponents to a measly 73 points per game over the two-week stretch.  Charles Barkley (18 PPG) and Michael Jordan (14.9 PPG) led the way in scoring.<br><br>
                                Below is a look at player contributions in each of their eight victories.</p>"),
                                   br()
                            ),
                            column(1)
                            # push text box on top of parallax image
               ), style = 'margin-top: -250px;'),
               
               br(),
               br(),
               br(),
               
               fluidRow(
                   HTML('<center>'),
                   highchartOutput('plot', height = '500px'),
                   HTML('</center>')
               ),
               
               fluidRow(id = 'text',
                        column(1),
                        column(10, 
                               br(),
                               HTML("<h2>The impact.</h2> 
                                    Lorem ipsum dolor sit amet, et malesuada conubia id, sapien et. Risus erat sagittis, nunc neque, scelerisque vel. Eu suspendisse lorem nullam eu risus feugiat ac lobortis eros? Dui ut mi nibh mauris urna neque sed, enim tempus tincidunt nunc ante eros ullamcorper et. In ipsum praesent vel faucibus tempus in congue ipsum lacinia. Facilisis volutpat vitae, dis, sagittis aenean nec ut. Pretium quis tincidunt rhoncus auctor enim erat nisi taciti ut donec eget. Quis, vehicula suscipit non ac netus nam facilisi. Facilisis inceptos velit dolor et fringilla luctus cras. Gravida nisl ut eget fusce fusce? At risus mauris urna non sodales ornare urna venenatis. Inceptos sociis tincidunt laoreet vitae. Id augue ut dignissim erat mi. Cras velit class vel ultrices ligula lacinia."
                            ), br(), br()),
                        column(1)
               ),
               br(),
               br(),
               hr(),
               br(),
               
               fluidRow(
                   column(2),
                   column(8,
                          HTML("<p><i>
                <span style='font-size:18px'><u>References</u></span><br>
                <span style='font-size:14px'>
                Data and photos were collected from several sites in order to create this web application, including
                <a href='https://www.nba.com' target='_blank'>nba.com</a> (career statistics, team photo, and player photos excluding J. Stockton and C. Laettner), 
                <a href='https://www.basketball-reference.com' target='_blank'>basketball-reference.com</a> (player photos of J. Stockton and C. Laettner), and 
                <a href='https://www.archive.fiba.com' target='_blank'>archive.fiba.com</a> (box scores for the 1992 Olympic men's basketball tournament). 
                The R packages powering this site include 
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://github.com/dreamRs/shinyWidgets' target='_blank'>shinyWidgets</a>,
                <a href='https://github.com/JohnCoene/scrollytell' target='_blank'>scrollytell</a>,
                <a href='https://plot.ly/r/' target='_blank'>plotly</a>,
                <a href='https://github.com/tidyverse/glue' target='_blank'>glue</a>, and 
                <a href='https://readxl.tidyverse.org/' target='_blank'>readxl</a>.
                </span>
                </i></p>")
                   ),
                   column(2)
               ),
               br(),
               br()
               
        ),
        column(1))
) # close UI fluidPage
