library(shiny)
library(tm)
library(SnowballC)
library(ggplot2)
library(ggdendro)
library(rsconnect)
set.seed(123)

# reading file and splitting Promoters vs Detractors
data <- read.csv("Book1.csv", stringsAsFactors = TRUE)
Promo <- as.data.frame(data[data$Score == "Prom", 2]); names(Promo) <- "Reason"
Detr <- as.data.frame(data[data$Score == "Detr", 2]); names(Detr) <- "Reason"

# Creating corpus
# Promo
Corpus_Promo <- Corpus(VectorSource(Promo$Reason))
Corpus_Promo <- tm_map(Corpus_Promo, content_transformer(tolower))
Corpus_Promo <- tm_map(Corpus_Promo, removePunctuation)
Corpus_Promo <- tm_map(Corpus_Promo, removeWords, stopwords("english"))

# Detr
Corpus_Detr <- Corpus(VectorSource(Detr$Reason))
Corpus_Detr <- tm_map(Corpus_Detr, content_transformer(tolower))
Corpus_Detr <- tm_map(Corpus_Detr, removePunctuation)
Corpus_Detr <- tm_map(Corpus_Detr, removeWords, stopwords("english"))

# Freq matrix (words >3 characters)
tdm_Promo <- TermDocumentMatrix(Corpus_Promo, control = list(wordLengths = c(3, Inf)))
tdm_Detr <- TermDocumentMatrix(Corpus_Detr, control = list(wordLengths = c(3, Inf)))

# Matrices
Promo_clust <- removeSparseTerms(tdm_Promo, sparse = .99)
Promo_matrix <- as.matrix(Promo_clust)
Detr_clust <- removeSparseTerms(tdm_Detr, sparse = .99)
Detr_matrix <- as.matrix(Detr_clust)


# Check for sparsity
# Promo
term.frq_P <- rowSums(as.matrix(tdm_Promo))

# Detract
term.frq_D <- rowSums(as.matrix(tdm_Detr))

ui <- navbarPage("Text analytics for customer feedback (TelCo Call centre example)",
                 tabPanel(title = "About",
                    p("This app has a simple objective: to expose the user to some simple text mining techniques, 
                    while allowing the interaction with three common Text Mining analyses: 
                    1) Word counting, 2) Tree clustering and 3) K-means for word proximity. As a reference, the input from a survey
                    of over two thousand entries was used, and both the client and participants remained confidential. 
                    Participants were asked to rate the service they received from a telecommunications company, 
                    during one month, and their answer were classified as positive (Promoters) or negative (Detractors)."),
                    br(),
                    p("Consequently, this app splits the input in two tabs: 
                    Promoters and Detractors **which can be selected from the top menu**. Under each tab 
                    there are three sliders allowing for the following interactions:"),
                    br(),
                    p("1) Bar chart with word count across all feedback: Choose lower percentage threshold
                    of word usage across all feedback (e.g. words used on at least 50 % of all feedback)."),
                    br(),
                    p("2) Dendogram of feedback’s clustering: Choose max allowed sparsity threshold
                    (i.e. resulting cluster contains only terms with a sparse factor of less than sparse value)."),
                    br(),
                    p("3) Verbatim of clusters by word proximity (K means): Choose number of words per cluster, 
                    to better understand narrative."),
                    br(),
                    p("According to IBM (2007), about 80% of the information created and used by an enterprise is 
                    unstructured data, and this figure is growing at twice the rate of structured data. 
                    Thus, it becomes imperative to develope the required skills and tools to unlock this vast
                    source of information for critical business insights.This app aims to expose the user to 
                    some of these techniques in an interactive and simple manner."),
                    br(),
                    p("This app was tested on Chrome Ver 44.0.2403.130 and Safari Ver 7.0.3, please ensure you have
                      the latest browser")
                    ),
                 # Promoter
                 tabPanel(title = "Promoters :o)",
                      wellPanel(sliderInput(inputId = "frq_P", 
                                  label = "1) Choose lower percentage threshold of word usage across all feedback", 
                                  value = 50, min = 20, max = 100, step = 10), 
                                  helpText("Count of frequent words used to provide feedback; 
                                           filtered by % usage across feedback 
                                           (e.g. words used on at least 50 % of all feedback)")),
                      plotOutput("bar_P"),
                      wellPanel(sliderInput(inputId = "spar_P", 
                                  label = "2) Choose max allowed sparsity threshold (i.e. resulting cluster contains only terms
                                  with a sparse factor of less than sparse value)", 
                                  value = .93, min = .88, max = .96, step = .01),
                                  helpText("Dendogram of feedback's word clustering")),
                      plotOutput("clu_P"),
                      wellPanel(numericInput(inputId = "story_P", 
                                             label = "3) Choose number of words per cluster", 
                                             value = 4, min = 1, max = 10, step = 1),
                                helpText("Verbatim of clusters by word proximity")),
                      verbatimTextOutput("Promoters")
                 ),
                 # Detractors
                tabPanel(title = "Detractors :o(",
                     wellPanel(sliderInput(inputId = "frq_D", 
                                           label = "1) Choose lower % threshold of word usage across all feedback", 
                                           value = 50, min = 20, max = 70, step = 10),
                                           helpText("Count of frequent words used to provide feedback; 
                                                    filtered by % usage across feedback 
                                                    (e.g. words used on at least 50 % of all feedback)")),
                     plotOutput("bar_D"),
                     wellPanel(sliderInput(inputId = "spar_D", 
                                           label = "2) Choose max allowed sparsity threshold (i.e. resulting cluster contains only terms
                                  with a sparse factor of less than sparse value)", 
                                           value = .93, min = .915, max = .94, step = .005),
                                           helpText("Dendogram of feedback's word clustering")),
                     plotOutput("clu_D"),
                     wellPanel(numericInput(inputId = "story_D", 
                                           label = "3) Choose number of words per cluster", 
                                           value = 4, min = 1, max = 10, step = 1),
                                           helpText("Verbatim of clusters by word proximity")),
                     verbatimTextOutput("Detractors")
                    )
               )

server <- function(input, output) {
     # Promoters
     output$bar_P <- renderPlot({
          # determine lower word frequency
          term.frq_P <- subset(term.frq_P, term.frq_P >= input$frq_P)
          df_P <- data.frame(term = names(term.frq_P), freq = term.frq_P)
          ggplot(df_P, aes(term, freq, fill = term)) + geom_bar(stat = "identity") +
               labs(title = "Promoters: Terms vs Freq", x = "Terms", y = "Count") + coord_flip() + theme_minimal() + theme(legend.position="none")
     })
     output$clu_P <- renderPlot({
          # remove sparse terms
          Promo_clust <- removeSparseTerms(tdm_Promo, sparse = input$spar_P)
          Promo_matrix <- as.matrix(Promo_clust)
          dist_Promo <- dist(scale(Promo_matrix))
          # clustering by K distance
          fit_Promo <- hclust(dist_Promo, method = "ward.D2")
          dendr    <- dendro_data(fit_Promo, type="rectangle")
          clust    <- cutree(fit_Promo,k=4)
          clust.df <- data.frame(label=names(clust), cluster=factor(clust))
          dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
          ggplot() + geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
               geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, color=cluster), 
                         size=4) + theme_minimal() + coord_flip() + scale_y_reverse(expand=c(.2,0)) +
               labs(title = "Dendogram: Clustering by themes", x = "Themes", y = "Distance")
          })
          # Story telling
          set.seed(123)
          output$Promoters <- renderPrint({ 
               P_story <- t(Promo_matrix)
               k <- 4
               kmeansResults_P <- kmeans(P_story, k)
               
               for (i in 1:k){
                    cat(paste("cluster ", i, ": ", sep = ""))
                    p <- sort(kmeansResults_P$centers[i, ], decreasing = T)
                    cat(names(p)[1:input$story_P], "\n")
               
                    }
          })
         
     # Detractors
     output$bar_D <- renderPlot({
          # determine lower word frequency
          term.frq_D <- subset(term.frq_D, term.frq_D >= input$frq_D)
          df_D <- data.frame(term = names(term.frq_D), freq = term.frq_D)
          ggplot(df_D, aes(term, freq, fill = term)) + geom_bar(stat = "identity") +
               labs(title = "Detractors: Terms vs Freq", x = "Terms", y = "Count") + coord_flip() + theme_minimal() + theme(legend.position="none")
          
     })
     output$clu_D <- renderPlot({
          # remove sparse terms
          Detr_clust <- removeSparseTerms(tdm_Detr, sparse = input$spar_D)
          Detr_matrix <- as.matrix(Detr_clust)
          # clustering by K distance
          dist_Detr <- dist(scale(Detr_matrix))
          fit_Detr <- hclust(dist_Detr, method = "ward.D2")
          dendr_D    <- dendro_data(fit_Detr, type="rectangle")
          clust_D    <- cutree(fit_Detr,k=4)
          clust.df_D <- data.frame(label=names(clust_D), cluster=factor(clust_D))
          dendr_D[["labels"]] <- merge(dendr_D[["labels"]],clust.df_D, by="label")
          ggplot() + geom_segment(data=segment(dendr_D), aes(x=x, y=y, xend=xend, yend=yend)) + 
               geom_text(data=label(dendr_D), aes(x, y, label=label, hjust=0, color=cluster), 
                         size=4) + theme_minimal() + coord_flip() + scale_y_reverse(expand=c(.2,0)) +
               labs(title = "Dendogram: Clustering by themes", x = "Themes", y = "Distance")
          })
          # Story telling    
          set.seed(123)
          output$Detractors <- renderPrint({ 
               D_story <- t(Detr_matrix)
               k <- 4
               kmeansResults_D <- kmeans(D_story, k)
               for (i in 1:k){
                    cat(paste("cluster ", i, ": ", sep = ""))
                    d <- sort(kmeansResults_D$centers[i, ], decreasing = T)
                    cat(names(d)[1:input$story_D], "\n")
               }
          })
}

shinyApp(ui = ui, server = server)