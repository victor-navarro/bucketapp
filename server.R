library(ggplot2)
source('theme_APA.R')
farthest = read.table("data/farthest.txt", header = T)
farthest$group = 'Reverse'
closest = read.table("data/closest.txt", header = T)
closest$group = 'Standard'
random = read.table("data/random.txt", header = T)
random$group = 'Random'
thedata = subset(rbind(farthest, closest, random), optimal_r != 9)



intro_text = list(h4(p("About the task:")), p("In the Bucket Task, pigeons are shown two possible paths on a touchscreen, like ", a("this.", href = 'https://raw.githubusercontent.com/victor-navarro/bucketapp/master/bucket.webm', target="_blank")), 
              p("They choose one of the paths by pecking a colored square, and after it, they had to peck the image of a bucket. Each peck made the bucket move one step towards the end of the path. 
                       Once the bucket reached the end of the path, one last peck produced food. The birds received multiple trials a day, in which the position of the buckets changed."),
              p("We manipulated, in a between-subjects design, the relationship between the position of the bucket and the amount of pecks required to move the bucket to the endzone. In the 'Standard' condition,
                                  the bucket that is the closest to the endzone requires less pecks, and the birds can reduce the response-cost by choosing it (it takes less effort!). In the 'Reverse' condition such
                                  relationship is inversed, such as it is now the farthest bucket the one that is optimal."), 
              p("You can see a video of the Standard group", a("here.", href = 'https://raw.githubusercontent.com/victor-navarro/bucketapp/master/standard.webm', target="_blank"), 
                "Also, you can see a video of the Reverse group", a("here.", href = 'https://raw.githubusercontent.com/victor-navarro/bucketapp/master/reversed.webm', target="_blank")),
              p("We trained the birds on those tasks for 15 days. After that, in order to explore what was driving the birds' behavior, we equalized the amount of work required across the buckets. In this
                                  'Equal work' phase, the number of pecks to move any of the buckets to the end goal was the same, regardless of their position. This phase disrupted their responding, making them lapse into 
                                  a position preference."),
              p("Finally, we retrained them in the original version of the task, to see if they could recover their initial performance. Did they do it? Find yourself! Play around with the buttons and sliders.
                              If you have trouble or find a bug, ", a("drop me a line!", href = "mailto:victor-navarro@uiowa.edu")),
              h4(p("How to use:")), p("Go crazy! You will be able to select different subsets of data. The main options panel will let you subset a chunk of the data as function of
                                           both groups, individual groups, and even individual birds within the groups, all at different points in time within each phase. The plots you will produce use two key variables:"), 
              p(strong("Distance"), " represents the difference between the buckets' starting positions, and was calculated as the starting position of the left bucket minus the starting position of the right bucket. 
                      Given that there were 10 posible starting positions (10 is the top), the range that this variable can take goes from -9 (left bucket at the bottom, right bucket at the top), to 9 (left bucket at the top, right bucket at the bottom)."),
              p(strong("Proportion of left choices"), "represents the proportion of trials on which the left bucket was chosen."))

#save the common layers
common_layers = list(stat_summary(geom = 'line', fun.y = 'mean'), geom_vline(xintercept = 0, linetype = 'dotted', alpha = .4), 
                     geom_hline(yintercept = 0.5, linetype = 'dotted', alpha = .4), 
                     stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = .2, alpha = .7), 
                     theme_APA(), stat_summary(fun.y = 'mean', geom = 'point', size = 3, fill = 'white'), 
                     labs(x = 'Distance (left bucket - right bucket)', y = 'Proportion of left choices'),
                     coord_cartesian(ylim = c(-0.02, 1.02)),  
                     scale_y_continuous(breaks = seq(0.0, 1, .1), labels = seq(0.0, 1, .1)), 
                     scale_x_continuous(breaks = c(-9:9)))

#group comparison layers
group_layers = list(scale_shape_manual(name = 'Group', values = c(21, 16)))

alignCenter <- function(el) {
    htmltools::tagAppendAttributes(el,
                                   style="margin-left:auto;margin-right:auto;"
    )
}

shinyServer(function(input, output) {
    output$instructions = renderUI({
        if(!is.null(input$hide)){
            if(input$hide %% 2 == 0){
            helpText(intro_text) 
            }
        }
    })

    output$hide = renderUI({
        if(is.null(input$instructions)){
            actionButton('hide', 'Show/Hide instructions')
        }
    })
    
    output$birds = renderUI({
        if (input$mode == "Individual"){
            if(input$group == 'Standard'){
                b_selection = c("24W", "54Y", "62B", "71R")
            }else if (input$group == 'Reverse'){
                b_selection = c("8R", "44W", "60Y", "83Y")
            }else{
                b_selection = c("11Y", "16W", "28Y", "36R")
            }
            radioButtons("bird", "Subject", choices = b_selection)
        }
    })
    
    output$block_s = renderUI({
        if(input$moment == 'Blocks of sessions'){
            numericInput('blocksize', label = 'Block size', min = 1, max = 15, value = 1)
        }
    })
    output$slider = renderUI({
        if(!is.null(input$range)){
            if(input$range){
                if(input$moment == 'Individual sessions'){
                    alignCenter(sliderInput("session", label = NULL, min = 1, max = 15, value = c(1, 15), step = 1, pre = "Session"))
                }else if(input$moment == 'Blocks of sessions'){
                    if (!is.null(input$blocksize)){
                        alignCenter(sliderInput("session", label = NULL, min = 1, max = ceiling(15/input$blocksize), value = c(1, ceiling(15/input$blocksize)), step = 1, pre = "Block"))
                    }
                }
            }else{
                if(input$moment == 'Individual sessions'){
                    alignCenter(sliderInput("session", label = NULL, min = 1, max = 15, value = 1, step = 1, pre = "Session"))
                }else if(input$moment == 'Blocks of sessions'){
                    if (!is.null(input$blocksize)){
                        alignCenter(sliderInput("session", label = NULL, min = 1, max = ceiling(15/input$blocksize), value = 1, step = 1, pre = "Block"))
                    }
                }
            }
        }
    })
    output$rangecheck = renderUI({
        if(input$moment == 'Individual sessions' | input$moment == 'Blocks of sessions'){
            alignCenter(checkboxInput('range', label = "Range selection", value = FALSE))
        }
    })
    output$plot = renderPlot({
        #select the data
        if(input$mode == 'Individual'){
                holder = subset(thedata, group == input$group & phase == input$phase & bird == input$bird)
        }else if (input$mode == 'Group'){
            holder = subset(thedata, group == input$group & phase == input$phase)
        }else{
            holder = subset(thedata, phase == input$phase)
        }
        
        #calculate blocks
        if(input$moment == 'Blocks of sessions'){
            if(!is.null(input$blocksize)){
                holder$block = ceiling(holder$psession/input$blocksize)
            }
        }
        
        #plotting depending on what we want to see
        if(input$mode == 'Group comparison'){
            if(input$comp == 'Std vs. Rev'){
                holder = subset(holder, group == 'Standard' | group == 'Reverse')
            }else if (input$comp == 'Std vs. Rand'){
                holder = subset(holder, group == 'Standard' | group == 'Random')
            }else{
                holder = subset(holder, group == 'Reverse' | group == 'Random')
            }
            summary(holder)
            if(input$moment == 'All sessions'){
                ggplot(holder, aes(x=distance, y = left, shape = group))  + common_layers + group_layers
            }else if(input$moment == 'Individual sessions'){
                if (!is.null(input$range)){
                    if (!input$range){
                        ggplot(subset(holder, psession == input$session), aes(x=distance, y = left, shape = group)) + common_layers + group_layers
                    }else{
                        ggplot(subset(holder, psession >= input$session[1] & psession <= input$session[2]), aes(x=distance, y = left, shape = group)) + 
                            common_layers + group_layers
                    }
                }
            }else if(input$moment == 'Blocks of sessions'){
                if(!is.null(input$blocksize)){
                    if (!is.null(input$range)){
                        if (!input$range){
                            ggplot(subset(holder, block == input$session), aes(x=distance, y = left, shape = group)) + common_layers + group_layers
                        }else{
                            ggplot(subset(holder, block >= input$session[1] & psession <= input$session[2]), aes(x=distance, y = left, shape = group)) + 
                                common_layers + group_layers
                        }
                    }
                }
            }
        }else{
            if(input$moment == 'All sessions'){
                ggplot(holder, aes(x=distance, y = left)) + common_layers
            }else if(input$moment == 'Individual sessions'){
                if (!is.null(input$range)){
                    if (!input$range){
                        ggplot(subset(holder, psession == input$session), aes(x=distance, y = left)) + common_layers
                    }else{
                        ggplot(subset(holder, psession >= input$session[1] & psession <= input$session[2]), aes(x=distance, y = left)) + common_layers
                    }
                }
            }else if(input$moment == 'Blocks of sessions'){
                if(!is.null(input$blocksize)){
                    if (!is.null(input$range)){
                        if (!input$range){
                            ggplot(subset(holder, block == input$session), aes(x=distance, y = left)) + common_layers
                        }else{
                            ggplot(subset(holder, block >= input$session[1] & psession <= input$session[2]), aes(x=distance, y = left)) + common_layers
                        }
                    }
                }
            }
        }
        }, bg="transparent")
})