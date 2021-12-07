## Homework 3

# Clean environment
rm(list=ls())

# Set up R with AWS

# Load libraries and install packages
library(aws.comprehend)
library(readtext)
library(tidyverse)
library(data.table)
# install.packages("kableExtra")
library(kableExtra)
library(dplyr)
library(ggplot2)
library(ggpubr)
# install.packages("forcats")
library(forcats)
library(modelsummary)

## Load Lord of The Rings books
b1 <- readtext('https://raw.githubusercontent.com/saravargha/ceu-data-engineering-2/main/data-raw/01%20-%20The%20Fellowship%20Of%20The%20Ring.txt')
b2 <- readtext('https://raw.githubusercontent.com/saravargha/ceu-data-engineering-2/main/data-raw/02%20-%20The%20Two%20Towers.txt')
b3 <- readtext('https://raw.githubusercontent.com/saravargha/ceu-data-engineering-2/main/data-raw/03%20-%20The%20Return%20Of%20The%20King.txt')

## Store the text in separate variables
lotr1 <- b1$text
lotr2 <- b2$text
lotr3 <- b3$text

## Clean text from unneccessary characters
lotr1 <- gsub("\n", " ", lotr1) 
lotr2 <- gsub("\n", " ", lotr2)
lotr3 <- gsub("\n", " ", lotr3)

## In order to run analysis on the three books, they needed to be split into smaller chunks.
## Start with the first book - The Fellowship of the Ring

# Add function to split the books into chunks
char.segments <- function(x, segm.length){
  byte.counter <- nchar(x, type = 'bytes')
  f <- c(1, rep(0, segm.length - 1))
  f <- cumsum(rep(f, length.out = byte.counter))
  s <- split(unlist(strsplit(x,'')), f)
  unname(sapply(s, paste, collapse = ''))
}

# Split the text into <5.000 byte chunks accordingly
lotr1_chunks <- char.segments(lotr1,4500)
# Create data frame for The Fellowship of the Ring:
lotr1_df <- data.table()

# Add Text column with the chunks
lotr1_df$Text <- t(rbind(lotr1_chunks))

# Create empty columns for the sentiment analysis outputs
lotr1_df$Index <- cbind("")
lotr1_df$Sentiment <- cbind("")
lotr1_df$Mixed <- cbind("")
lotr1_df$Negative <- cbind("")
lotr1_df$Neutral <- cbind("")
lotr1_df$Positive <- cbind("")
lotr1_df$Book <- cbind("")


# Run sentiment analysis
for (i in 1:nrow(lotr1_df)) {
  t <- detect_sentiment(lotr1_df$Text[i])
  lotr1_df$Index[i] <- i
  lotr1_df$Sentiment[i] <- t[[2]]
  lotr1_df$Mixed[i] <- t[[3]]
  lotr1_df$Negative[i] <- t[[4]]
  lotr1_df$Neutral[i] <- t[[5]]
  lotr1_df$Positive[i] <- t[[6]]
  lotr1_df$Book[i] <- "LOTR1"
}

#Repeat the above steps for The Two Towers
lotr2_chunks <- char.segments(lotr2,4500)

lotr2_df <- data.table()

lotr2_df$Text <- t(rbind(lotr2_chunks))

lotr2_df$Index <- cbind("")
lotr2_df$Sentiment <- cbind("")
lotr2_df$Mixed <- cbind("")
lotr2_df$Negative <- cbind("")
lotr2_df$Neutral <- cbind("")
lotr2_df$Positive <- cbind("")
lotr2_df$Book <- cbind("")

for (i in 1:nrow(lotr2_df)) {
    t <- detect_sentiment(lotr2_df$Text[i])
    lotr2_df$Index[i] <- i
    lotr2_df$Sentiment[i] <- t[[2]]
    lotr2_df$Mixed[i] <- t[[3]]
    lotr2_df$Negative[i] <- t[[4]]
    lotr2_df$Neutral[i] <- t[[5]]
    lotr2_df$Positive[i] <- t[[6]]
    lotr2_df$Book[i]<- "LOTR2"
  }


## Repeat the same steps for The Return of the King
lotr3_chunks <- char.segments(lotr3,4500)

lotr3_df <- data.table()

lotr3_df$Text <- t(rbind(lotr3_chunks))

lotr3_df$Index <- cbind("")
lotr3_df$Sentiment <- cbind("")
lotr3_df$Mixed <- cbind("")
lotr3_df$Negative <- cbind("")
lotr3_df$Neutral <- cbind("")
lotr3_df$Positive <- cbind("")
lotr3_df$Book <- cbind("")

for (i in 1:nrow(lotr3_df)) {
  t <- detect_sentiment(lotr3_df$Text[i])
  lotr3_df$Index[i] <- i
  lotr3_df$Sentiment[i] <- t[[2]]
  lotr3_df$Mixed[i] <- t[[3]]
  lotr3_df$Negative[i] <- t[[4]]
  lotr3_df$Neutral[i] <- t[[5]]
  lotr3_df$Positive[i] <- t[[6]]
  lotr3_df$Book[i] <- "LOTR3"
}

### Sentiment analysis of the entire series
  
lotrglobal_df <- rbind(lotr1_df, lotr2_df, lotr3_df)

# Set the type of variables
lotrglobal_df$Index <- as.numeric(lotrglobal_df$Index)
lotrglobal_df$Mixed <- as.numeric(lotrglobal_df$Mixed)
lotrglobal_df$Negative <- as.numeric(lotrglobal_df$Negative)
lotrglobal_df$Positive <- as.numeric(lotrglobal_df$Positive)
lotrglobal_df$Neutral <- as.numeric(lotrglobal_df$Neutral)
lotrglobal_df$Book <- as.factor(lotrglobal_df$Book)

# Rewrite indexing
for (i in 228:nrow(lotrglobal_df)) {
  lotrglobal_df$Index[i] <- i
}

# Check data frame
lotrglobal_df %>% 
  kbl() %>%
  kable_styling() %>% 
  head(1)

# Analysis & visualization
v1 <- ggplot(lotrglobal_df, aes(x = Index, color=Sentiment)) +
  geom_smooth(aes(y = Positive), method = "loess", color = "#02818a", size=2) +
  geom_smooth(aes(y = Mixed), method = "loess", color = "#bdc9e1", size=2) +
  geom_smooth(aes(y= Negative), method = "loess", color = "#f6eff7", size=2) +
  labs(y = "Sentiment", x= "Storyline") +
  scale_x_continuous(breaks = c(0, 228, 413,574)) + ## Chunks: LOTR1 1-227, LOTR2 228-413, LOTR 3 414-574
  geom_vline(xintercept = 228, linetype="dashed") +
  geom_vline(xintercept= 413, linetype="dashed") +
  annotate("text", x = 120, y = 0.25, label ="The Fellowship of the Ring", size=4)+
  annotate("text", x = 320, y = 0.25, label ="The Two Towers", size=4) +
  annotate("text", x = 500, y = 0.25, label ="The Return of the King", size=4)+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "gray95",
                                    size = 0.5, linetype = "solid"),
    legend.position = "right", 
    legend.title = element_text("Sentiment"), 
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank())
v1



v2 <- lotrglobal_df %>%
  group_by(Book, Sentiment) %>%
  summarize("Sentiment_count"=n()) %>% 
  mutate("Sentiment_prop"=Sentiment_count/sum(Sentiment_count)*100) %>% 
  ggplot(aes(x=Book, y=Sentiment_prop, fill=factor(Sentiment, levels=c("POSITIVE", "NEUTRAL", "MIXED", "NEGATIVE")))) +
  geom_bar(position="fill", stat="identity", width=0.5, color="white", lwd=2) +
  scale_x_discrete(labels=c("LOTR1" = "The Fellowship of the Ring", "LOTR2" = "The Two Towers", "LOTR3" = "The Return of the King")) +
  scale_fill_brewer(palette="PuBuGn", direction=-1) +
  coord_flip() +
  labs(x="Books", y="Sentiment ratio", fill="Sentiment") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "gray95",
                                    size = 0.5, linetype = "solid"),
    legend.position = "bottom", 
    legend.title = element_text("Sentiment"),
    axis.text.x = element_text(angle=0))
v2

v3 <- lotrglobal_df %>%
  group_by(Book, Sentiment) %>%
  summarize("Sentiment_count"=n()) %>% 
  mutate("Sentiment_prop"=Sentiment_count/sum(Sentiment_count)*100) %>%           
  ggplot(aes(x=Book, y=Sentiment_prop, fill=factor(Sentiment, levels=c("POSITIVE", "NEUTRAL", "MIXED", "NEGATIVE")))) +
  geom_col(color='white', show.legend = F, lwd=2) +
  geom_text(aes(label = round(Sentiment_prop, digits=1)), 
            size = 3, 
            position = position_dodge(1),
            vjust = 0)+
  facet_grid(~ Sentiment)+
  labs(x="Books", y="Sentiment %") +
  scale_x_discrete(labels=c("LOTR1" = "The Fellowship of the Ring", "LOTR2" = "The Two Towers", "LOTR3" = "The Return of the King")) +
  scale_fill_brewer(palette="PuBuGn", direction=-1) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "gray95",
                                    size = 0.5, linetype = "solid"),
    legend.position = "bottom",
    legend.title = element_text("Sentiment"),
    axis.text.x = element_text(angle=90, size=10, vjust=0.6),
    axis.text.y=element_blank(),
    axis.ticks = element_blank()
  )
v3

v4 <- lotrglobal_df %>%
  group_by(Book, Sentiment) %>%
  summarize("Sentiment_count"=n()) %>% 
  mutate("Sentiment_prop"=Sentiment_count/sum(Sentiment_count)*100) %>%
  ggplot(aes(x=Book, y=Sentiment_prop, fill=factor(Sentiment, levels=c("POSITIVE", "NEUTRAL", "MIXED", "NEGATIVE")))) +
  geom_bar(position="dodge", stat="identity", color='white', lwd=2) +
  geom_text(aes(label = round(Sentiment_prop, digits=1)), 
            size = 3, 
            position = position_dodge(1),
            vjust = 0)+
  labs(y="Sentiment %") +
  scale_x_discrete(labels=c("LOTR1" = "The Fellowship of the Ring", "LOTR2" = "The Two Towers", "LOTR3" = "The Return of the King")) +
  scale_fill_brewer(palette="PuBuGn", direction=-1) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "gray95",
                                    size = 0.5, linetype = "solid"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle=0),
    axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks = element_blank()
  )
v4

  
