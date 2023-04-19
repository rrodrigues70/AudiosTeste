#######################################################################################################
#
# Description: Determine Word Frequency of a Text File
#
# Location: N/A
#
# Program name: N/a
# 
# Source code: v1.0
#
# Author: Jason Watts
#
# Sys.info: SnowWhite and the 88 Dwarfs
#
# Computational Framework: Microsoft R Open version: >=3.4.2
#
# Web Framework: RStudio - N/A

# Analytics Dashboard Framework: N/A
#
# Plotting and Graphics: Plotly: ggplot2: >=2.2.1
# 
# License: Private with Open Source components. Open Source components require credits with distribution.  
#######################################################################################################

# Load Required Libraries

library(ggplot2)
library(tm)
library(tau)
library(plyr)
library(dplyr)
library(readr)
library(plotly)

# Set Minimum and Maximum Word Frequency
a <- 2
b <- 1000

# Remove Stop Words - T/F (True/False)
stop_words<-T

# Download and Read Text File

data <- tm::PlainTextDocument(readr::read_lines(file = "C:/temp/L1.txt", 
                                                progress = interactive()), heading = "KJB", id = basename(tempfile()), 
                              language = "pt", description = "Report File")

data2 <- tm::PlainTextDocument(readr::read_lines(file = "C:/temp/ExclusaoOriginalPortuguese.txt", 
                                                 progress = interactive()), heading = "KJB", id = basename(tempfile()), 
                               language = "pt", description = "Report File")

# Remove words

data=removeWords(data,c(data2$content,"Eu", "Taqui")) 

data <- tau::textcnt(
  
  if(stop_words==T) {tm::removeWords(tm::scan_tokenizer(data), tm::stopwords("pt"))}
  
  else {
    
    tm::scan_tokenizer(data)
  }
  
  , method = "string", n = 1L, lower = 1L)

# Change List to Data Frame
data <- plyr::ldply(data, data.frame) 

# Using dplyr Filter
Results<-dplyr::filter(data, data[,2]>a & data[,2]<b)

colnames(Results)<-c("word", "frequency")

ggplot2::ggplot(Results, aes(x=word, y=frequency, fill=word)) + geom_bar(width = 0.75,  stat = "identity", colour = "black", size = 1) + coord_polar(theta = "x") + xlab("") + ylab("") + ggtitle("Word Frequency") + theme(legend.position = "none") + labs(x = NULL, y = NULL)

plotly::ggplotly(ggplot2::ggplot(Results, aes(x=word, y=frequency, fill=word)) + geom_bar(width = 0.75, stat = "identity", colour = "black", size = 1) + 
                   xlab("") + ylab("") + ggtitle("Word Frequency") + theme(legend.position = "none") + labs(x = NULL, y = NULL) + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), axis.text.x = element_text(angle = 90)) + theme(panel.background = element_rect(fill = "honeydew1"), plot.background = element_rect(fill = "antiquewhite")))%>% config(displaylogo = F) %>% config(showLink = F)
