library(shiny)
library(ggfortify)
library(ggplot2)
library(DT)
library(distributions3)
library(stringr)
library(EnvStats)
library(tidyverse)
library(ggpubr)
library(plyr)
library(dplyr)
library(tidyr)

options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, clientData, session) {
  load("data.Rda")
  data <- reactiveVal(list())
  values <- reactiveValues()
  
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- (cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) cex <- 0.8 / strwidth(txt)
    
    test <- cor.test(x, y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value,
                     corr = FALSE, na = FALSE,
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                     symbols = c("***", "**", "*", ".", " ")
    )
    
    text(0.5, 0.5, txt, cex = cex * abs(r))
    text(.8, .8, Signif, cex = cex, col = 2)
  }
  
  observeEvent(input$Panel, {
    updateSelectInput(session, "var1", choices = names(data()))
    updateSelectInput(session, "svar", choices = names(data()))
    updateSelectInput(session, "oneplusvar1", choices = names(data()))
    updateSelectInput(session, "oneplusvar2", choices = names(data())[-which(names(data()) == input$oneplusvar1)])
    updateSelectizeInput(session, "var2", choices = names(data())[-which(names(data()) == input$var1)])
  })
  
  # We can visit later
  observeEvent(input$svar, {
    values$degf <- 10
  })
  
  output$which.server <- renderUI({
    if (input$f.choice != "server") {
      return()
    }
    radioButtons("which.server", "Select a Server: ", c("MATH 1700" = "1700", "MATH 4720" = "4720", "Default" = ""), selected = "", inline = TRUE, width = "350px")
  })
  
  output$s.choice <- renderUI({
    if (input$f.choice != "server" || is.null(input$which.server)) {
      return()
    }
    if (input$which.server == "") {
      s.choices <- 1:length(data.serv)
      names(s.choices) <- names(data.serv)
    } else {
      s.choices <- dir(input$which.server)
      s.choices <- substr(s.choices, 1, nchar(s.choices) - 4)
      names(s.choices) <- s.choices
    }
    selectInput("s.choice", "Select a file from server: ", c(Choose = "", s.choices), selectize = TRUE, width = "250px")
  })
  
  output$file <- renderUI({
    if (input$f.choice != "upload") {
      return()
    }
    fileInput("file", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
  })
  output$sep <- renderUI({
    if (input$f.choice != "upload") {
      return()
    }
    radioButtons("sep", "Separator", c("," = ",", ":" = ":", ";" = ";", Tab = "\t"), ",", inline = TRUE)
  })
  output$header <- renderUI({
    if (input$f.choice != "upload") {
      return()
    }
    checkboxInput("header", "Header", TRUE)
  })
  
  output$ts.selected <- renderText({
    if (input$f.choice == "upload" && is.null(input$file)) {
      return("<b>Select a 'csv' file that contain the variables in its columns</b>")
    }
    text <- paste("<b>", ncol(data()), "Variables of length", nrow(data()), "</b>")
    return(text)
  })
  
  output$dynamic <- renderDataTable(
    {
      if ((input$f.choice == "upload" && is.null(input$file)) || (input$f.choice == "server" && (input$s.choice == "" || is.null(input$s.choice) || is.null(input$which.server)))) {
        return()
      }
      if (input$f.choice == "upload") {
        tmp <- as.data.frame(read.table(input$file$datapath, header = input$header, sep = input$sep))
        tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], as.factor)
        data(tmp)
      } else if (input$which.server == "") {
        i <- as.numeric(input$s.choice)
        data(data.serv[[i]])
      } else {
        tmp <- as.data.frame(read.table(paste0(input$which.server, "/", input$s.choice, ".txt"), header = TRUE, sep = ","))
        tmp[sapply(tmp, is.character)] <- lapply(tmp[sapply(tmp, is.character)], as.factor)
        data(tmp)
      }
      if (is.null(colnames(data()))) {
        colnames(data()) <- c("y", paste("fn", 1:(ncol(data()) - 1)))
      }
      return(data())
    },
    options = list(pageLength = 10)
  )
  
  output$data.plot <- renderPlot({
    if (memory.size() > 700) gc()
    if ((input$f.choice == "upload" && is.null(input$file)) || is.null(ncol(data()))) {
      return()
    }
    if (ncol(data()) > 1) {
      pairs(data(), upper.panel = panel.smooth, lower.panel = panel.cor)
    } else {
      plot(data()[2:1], asp = input$asp)
      abline(h = 0, v = 0, lty = 2)
    }
  })
  
  #### DESCRIPTIVE STATISTICS
  
  output$var1 <- renderUI({
    selectizeInput("var1", "Variable 1", choices = names(data()))
  })
  
  output$var2 <- renderUI({
    if (input$i2ndVar == FALSE) {
      return()
    }
    selectizeInput("var2", "Variable 2", choices = names(data())[-which(names(data()) == input$var1)], multiple = FALSE)
  })
  
  output$quant <- renderUI({
    if (is.null(ncol(data()))) {
      return()
    }
    if (is.numeric(data()[, input$var1]) | is.numeric(data()[, input$var2])) {
      sliderInput("quant", "Percentile", min = 0.00, max = 1, value = 0.5, step = .01, width = "150px")
    }
  })
  
  output$desFig <- renderUI({
    if (is.null(ncol(data()))) {
      return()
    }
    #browser()
    if (input$i2ndVar == FALSE && is.factor(data()[, input$var1])) {
      options <- c("None" = "None", "Bar Graph" = "barG", "Pie Chart" = "Pchart")
      selectizeInput("desFig", "Descriptive Figure: ", choices = c("Bar Graph" = "barG", "Pie Chart" = "Pchart"), selected = "barG", width = "250px")
    } else if (input$i2ndVar == FALSE && is.numeric(data()[, input$var1])) {
      selectizeInput("desFig", "Descriptive Figure: ", choices = c("Histogram" = "histp", "Box Plot" = "bxplt"), selected = "hisp", width = "250px")
    } else if (input$i2ndVar == TRUE && ((is.factor(data()[, input$var1]) && is.numeric(data()[, input$var2])) || (is.factor(data()[, input$var2]) && is.numeric(data()[, input$var1])))) {
      selectizeInput("desFig", "Descriptive Figure: ", choices = c("Side By Side Boxplot" = "sbsbxplt"), selected = "sbsbxplt", width = "250px")
    } else if (input$i2ndVar == TRUE && ((is.numeric(data()[, input$var1]) && is.numeric(data()[, input$var2])))) {
      selectizeInput("desFig", "Descriptive Figure: ", choices = c("Scatter Plot" = "sctplt"), selected = "sctplt", width = "250px")
    } else if (input$i2ndVar == TRUE && ((is.factor(data()[, input$var1]) && is.factor(data()[, input$var2])))) {
      selectizeInput("desFig", "Descriptive Figure: ", choices = c("Baloon Plot" = "balplt"), selected = "balplt", width = "250px")
    }
  })
  
  output$des.summ <- renderPrint({
    if (is.null(ncol(data()))) {
      return()
    }
    if (input$insumstats == FALSE) {
      return("Check Include Summary Statistics")
    } else if (input$insumstats == TRUE & input$i2ndVar == FALSE) {
      sumar <- list()
      sumar$summary <- summary(data()[, input$var1])
      if (is.numeric(data()[, input$var1])) {
        sumar$percentile <- stats::quantile(data()[, input$var1], input$quant)
      }
    } else if (input$insumstats == TRUE & input$i2ndVar == TRUE) {
      sumar <- list()
      lst <- list(data()[, input$var1], data()[, input$var2])
      sumar$summary <- map(lst, summary)
      if (is.numeric(data()[, input$var1])) {
        sumar[[input$var1]] <- stats::quantile(data()[, input$var1], input$quant)
      }
      if (is.numeric(data()[, input$var2])) {
        sumar[[input$var2]] <- stats::quantile(data()[, input$var2], input$quant)
      }
    }
    sumar
  })
  
  ##  Descriptive Figures
  
  output$nbin <- renderUI({
    if (is.null(input$desFig)) {
      return()
    } else if (input$desFig == "histp") {
      sliderInput("nbin", "Number of bins", min = 1, max = 100, value = length(hist(data()[, input$var1], plot=FALSE)$breaks)-1, step = 1)
    }
  })
  output$des.plot <- renderPlot({
    if (is.null(input$desFig)) {
      return()
    } else if (input$desFig == "histp") {
      hist(data()[, input$var1], breaks = input$nbin, main = "Histogram", xlab = input$var1)
      abline(v = stats::quantile(data()[, input$var1], input$quant), col = 2, lty = 2)
    } else if (input$desFig == "bxplt") {
      boxplot(data()[, input$var1], ylab = input$var1, main = "Box Plot")
      abline(h = stats::quantile(data()[, input$var1], input$quant), col = 2, lty = 2)
    } else if (input$desFig == "barG") {
      counts <- table(data()[, input$var1])
      barplot(counts, main = input$var1)
    } else if (input$desFig == "Pchart") {
      mytable <- table(data()[, input$var1])
      lbls <- paste(names(mytable), "\n", mytable, sep = "")
      pie(mytable, labels = lbls, main = input$var1)
    } else if (input$desFig == "sbsbxplt") {
      if (is.numeric(data()[, input$var1])) {
        boxplot(data()[, input$var1] ~ data()[, input$var2], main = "Side-by-Side Box Plot", xlab = input$var2, ylab = input$var1)
        abline(h = stats::quantile(data()[, input$var1], input$quant), col = 2, lty = 2)
      } else {
        boxplot(data()[, input$var2] ~ data()[, input$var1], main = "Side-by-Side Box Plot", xlab = input$var1, ylab = input$var2)
        abline(h = stats::quantile(data()[, input$var2], input$quant), col = 2, lty = 2)
      }
    } else if (input$desFig == "sctplt") {
      plot(data()[, input$var1], data()[, input$var2], xlab = input$var1, ylab = input$var2, main = "Scatter Plot")
      perc1 <- stats::quantile(data()[, input$var1], input$quant)
      perc2 <- stats::quantile(data()[, input$var2], input$quant)
      segments(c(perc1, min(data()[, input$var1])), c(min(data()[, input$var2]), perc2), perc1, perc2, col = 2, lty = 2)
    } else if (input$desFig == "balplt") {
      ggballoonplot(data.frame(table(data()[, input$var1], data()[, input$var2])), fill = "value") + scale_fill_viridis_c(option = "C")
    }
  })
  
  ####  ONE SAMPLE
  
  output$svar <- renderUI({
    selectizeInput("svar", "Select Variable", choices = names(data()))
  })
  
  output$type <- renderUI({
    if (is.null(ncol(data()))) {
      return()
    }
    if (is.numeric(data()[, input$svar])) {
      selectInput("type", "Test for:", choices = c("Mean" = "mu", "Variance" = "sigma"), selected = "Mean", width = "210px")
    } else {
      selectInput("type", "Test for:", choices = c("Proportion" = "prop"), selected = "Proportion", width = "210px")
    }
  })
  
  output$dif.tst <- renderUI({
    if (is.null(input$type) || input$type != "mu") {
      return()
    } else if (input$type == "mu") {
      options <- c("t-Test" = "ttst", "Z-test" = "ztst")
    }
    selectizeInput("dif.tst", "Test:", choices = options, width = "150px")
  })
  
  ### Hypothesis Plots
  output$HT_plts <- renderUI({
    if (is.null(input$type)) {
      return()
    } else if (input$type == "mu" || input$type == "prop" || input$type == "sigma") {
      radioButtons("HT_plts", "Plots", c("Hypothesis Test (HT): Critical Region" = "HT_CR", "Confidence Interval - HT: p-value" = "HT_pval"), selected = "HT_CR", inline = TRUE)
    }
  })
  
  ## Confidence Interval
  output$confpval <- renderUI({
    if (input$HT_plts == "HT_CR" || is.null(ncol(data()))) {
      return()
    }
    checkboxGroupInput("confpval", NULL, c("Confidence Interval" = "conf", "Hypothesis Test (HT): p-value" = "pval"), selected = "conf")
  })
  
  ### Null hypothesis
  output$nullhypo <- renderUI({
    if (is.null(input$type)) {
      return()
    } else if (input$type == "mu") {
      se <- sd(data()[, input$svar]) / sqrt(nrow(data()))
      mu0 <- round(mean(data()[, input$svar]) + se, 2)
      numericInput("nullhypo", HTML("H<sub>0</sub>:<i>&mu; ="), value = mu0, step = max(0.01, round(.1 * se, 2)), width = "150px")
    } else if (input$type == "sigma") {
      va <- var(data()[, input$svar])
      sigma20 <- round(1.15 * va, 2)
      numericInput("nullhypo", HTML("H<sub>0</sub>:<i>&sigma;<sup>2</sup> ="), value = sigma20, step = max(0.01, round(.01 * va, 2)), min = 0.01, width = "150px")
    } else if (input$type == "prop") {
      numericInput("nullhypo", HTML("H<sub>0</sub>:<i>p ="), value = 0.5, step = .01, width = "150px", min = 0.01, max = 0.99)
    }
  })
  
  ### Alternate Hypothesis
  output$althypo <- renderUI({
    if (is.null(input$type)) {
      return()
    } else if (input$type == "mu") {
      selectInput("althypo", HTML("H<sub>a</sub>:<i>&mu;"), choices = c("!=" = "two.sided", "<" = "less", ">" = "greater"), width = "50px")
    } else if (input$type == "sigma") {
      selectInput("althypo", HTML("H<sub>a</sub>:<i>&sigma;<sup>2</sup>"), choices = c("!=" = "two.sided", "<" = "less", ">" = "greater"), width = "50px")
    } else if (input$type == "prop") {
      selectInput("althypo", HTML("H<sub>a</sub>:<i>p"), choices = c("!=" = "two.sided", "<" = "less", ">" = "greater"), width = "50px")
    }
  })
  
  output$sigma.squared <- renderUI({
    if (is.null(input$type) || is.null(input$dif.tst)) {
      return()
    }
    if (input$type == "mu" && input$dif.tst == "ztst") {
      numericInput("sigma.squared", "Population Variance:", HTML("<i>&sigma;<sup>2</sup> ="), value = 1, width = "75px")
    }
  })
  
  output$fctpptn <- renderUI({
    if (is.null(input$type)) {
      return()
    }
    if (input$type == "prop") {
      selectInput("fctpptn", "Test for Proportion of:", choices = c(levels(data()[, input$svar])), width = "210px")
    }
  })
  
  output$onesamtst <- renderPrint({
    values$alpha <- 1 - input$conflev
    if (is.null(input$type) || is.null(input$althypo) || (input$type == "mu" && is.null(input$dif.tst))) {
      return()
    }
    if (input$type == "mu" && input$dif.tst == "ttst") {
      values$tst <- ttst <- t.test(data()[, input$svar], mu = input$nullhypo, conf.level = input$conflev, alternative = input$althypo)
      values$degf <- ttst$parameter
      if (input$althypo == "two.sided") {
        values$crit_val <- qt(input$conflev + values$alpha / 2, df = values$degf, lower.tail = TRUE)
        values$pcrit_val <- qt(ttst$p.value / 2, df = values$degf, lower.tail = FALSE)
        values$vcrit_val <- c(-values$crit_val, values$crit_val)
      } else if (input$althypo == "less") {
        values$crit_val <- qt(input$conflev, df = values$degf, lower.tail = TRUE)
        values$pcrit_val <- qt(ttst$p.value, df = values$degf, lower.tail = FALSE)
        values$vcrit_val <- c(-values$crit_val, 0)
      } else if (input$althypo == "greater") {
        values$crit_val <- qt(input$conflev, df = values$degf, lower.tail = TRUE)
        values$pcrit_val <- qt(ttst$p.value, df = values$degf, lower.tail = FALSE)
        values$vcrit_val <- c(0, values$crit_val)
      }
      values$val <- (ttst$estimate * (sd(data()[, input$svar]) / sqrt(length(data()[, input$svar])))) + input$nullhypo
      values$xbar <- ttst$estimate
      values$LCL <- ttst$conf.int[1]
      values$UCL <- ttst$conf.int[2]
      values$pvalue <- ttst$p.value
      values$stats <- ttst$statistic
      ttst
    } else if (input$type == "mu" && input$dif.tst == "ztst") {
      zzlst <- list()
      values$tst <- values$stats <- zzlst$z_stat <- c("Z" = (mean(data()[, input$svar]) - input$nullhypo) / sqrt(input$sigma.squared / length(data()[, input$svar])))
      Z <- Normal(0, 1)
      values$tsthat <- mean(data()[, input$svar])
      if (input$althypo == "two.sided") {
        values$pvalue <- zzlst$pvalue <- 2 * cdf(Z, min(isolate(values$stats), -isolate(values$stats)))
        values$crit_val <- qnorm(input$conflev + values$alpha / 2)
        values$pcrit_val <- qnorm(zzlst$pvalue / 2)
        values$vcrit_val <- c(-values$crit_val, values$crit_val)
        values$LCL <- zzlst$LCL <- values$tsthat - values$crit_val * sqrt(input$sigma.squared / length(data()[, input$svar]))
        values$UCL <- zzlst$UCL <- values$tsthat + values$crit_val * sqrt(input$sigma.squared / length(data()[, input$svar]))
      } else if (input$althypo == "less") {
        values$pvalue <- zzlst$pvalue <- cdf(Z, isolate(values$stats))
        values$crit_val <- qnorm(input$conflev)
        values$pcrit_val <- qnorm(zzlst$pvalue)
        values$vcrit_val <- c(-values$crit_val, 0)
        values$LCL <- zzlst$LCL <- -Inf
        values$UCL <- zzlst$UCL <- values$tsthat + values$crit_val * sqrt(input$sigma.squared / length(data()[, input$svar]))
      } else if (input$althypo == "greater") {
        values$pvalue <- zzlst$pvalue <- 1 - cdf(Z, isolate(values$stats))
        values$crit_val <- qnorm(input$conflev)
        values$pcrit_val <- qnorm(zzlst$pvalue)
        values$vcrit_val <- c(0, values$crit_val)
        values$LCL <- zzlst$LCL <- values$tsthat - values$crit_val * sqrt(input$sigma.squared / length(data()[, input$svar]))
        values$UCL <- zzlst$UCL <- Inf
      }
      values$val <- (zzlst$z_stat * input$sigma.squared) + input$nullhypo
      values$xbar <- mean(data()[, input$svar])
      zzlst
    } else if (input$type == "prop") {
      pplst <- list()
      values$tsthat <- table(data()[, input$svar])[input$fctpptn] / length(data()[, input$svar])
      Z <- Normal(0, 1)
      values$stats <- pplst$pptst <- c("Z" = (values$tsthat - input$nullhypo) / sqrt((values$tsthat * (1 - values$tsthat)) / length(data()[, input$svar])))
      if (input$althypo == "two.sided") {
        values$pvalue <- pplst$pvalue <- 2 * cdf(Z, min(isolate(values$stats), -isolate(values$stats)))
        values$crit_val <- qnorm(input$conflev + values$alpha / 2)
        values$pcrit_val <- qnorm(values$pvalue / 2)
        values$vcrit_val <- c(-values$crit_val, values$crit_val)
        E <- values$crit_val * sqrt((values$tsthat * (1 - values$tsthat)) / length(data()[, input$svar]))
        values$LCL <- pplst$LCL <- values$tsthat - E
        values$UCL <- pplst$UCL <- values$tsthat + E
      } else if (input$althypo == "less") {
        values$pvalue <- pplst$pvalue <- cdf(Z, isolate(values$stats))
        values$crit_val <- qnorm(input$conflev)
        values$pcrit_val <- qnorm(values$pvalue)
        values$vcrit_val <- c(-values$crit_val, 0)
        E <- values$crit_val * sqrt((values$tsthat * (1 - values$tsthat)) / length(data()[, input$svar]))
        values$LCL <- pplst$LCL <- -Inf
        values$UCL <- pplst$UCL <- values$tsthat + E
      } else if (input$althypo == "greater") {
        values$pvalue <- pplst$pvalue <- 1 - cdf(Z, isolate(values$stats))
        values$crit_val <- qnorm(input$conflev)
        values$pcrit_val <- qnorm(values$pvalue)
        values$vcrit_val <- c(0, values$crit_val)
        E <- values$crit_val * sqrt((values$tsthat * (1 - values$tsthat)) / length(data()[, input$svar]))
        values$LCL <- pplst$LCL <- values$tsthat - E
        values$UCL <- pplst$UCL <- Inf
      }
      pplst
    } else if (input$type == "sigma") {
      if (input$althypo == "two.sided") {
        values$tst <- sigmatst <- varTest(data()[, input$svar], alternative = "two.sided", sigma.squared = input$nullhypo, conf.level = input$conflev)
        values$degf <- sigmatst$parameter
        values$vcrit_val <- c(qchisq(values$alpha / 2, values$degf), qchisq(input$conflev + values$alpha / 2, values$degf))
        values$pvalue <- sigmatst$p.value
        values$vpcrit <- c(qchisq(values$pvalue / 2, values$degf, lower.tail = T), qchisq(values$pvalue / 2, values$degf, lower.tail = F))
      } else if (input$althypo == "less") {
        values$tst <- sigmatst <- varTest(data()[, input$svar], alternative = "less", sigma.squared = input$nullhypo, conf.level = input$conflev)
        values$crit_val <- qchisq(values$alpha, length(data()[, input$svar]) - 1)
        values$vcrit_val <- c(values$crit_val, 0)
        values$pvalue <- sigmatst$p.value
        values$vpcrit <- c(qchisq(values$pvalue, values$degf, lower.tail = T), 0)
      } else if (input$althypo == "greater") {
        values$tst <- sigmatst <- varTest(data()[, input$svar], alternative = "greater", sigma.squared = input$nullhypo, conf.level = input$conflev)
        values$crit_val <- qchisq(input$conflev, length(data()[, input$svar]) - 1)
        values$vcrit_val <- c(0, values$crit_val)
        values$pvalue <- sigmatst$p.value
        values$vpcrit <- c(0, qchisq(values$pvalue, values$degf, lower.tail = F))
      }
      values$tsthat <- var(data()[, input$svar])
      values$ssquared <- sigmatst$estimate
      values$degf <- sigmatst$parameter
      values$LCL <- sigmatst$conf.int[1]
      values$UCL <- sigmatst$conf.int[2]
      values$stats <- sigmatst$statistic
      sigmatst
    }
  })
  
  output$onesamtst.plt <- renderPlot({
    if (is.null(input$type) || is.null(input$althypo)) {
      return()
    }
    values$alternative <- input$althypo
    values$statsP <- values$stats
    if (input$type == "sigma") values$nullhypo <- nrow(data()) - 1 else values$nullhypo <- 0
    if (values$alternative == "two.sided") values$newalpha <- values$alpha / 2 else values$newalpha <- values$alpha
    if (input$type == "prop" || (input$type == "mu" && input$dif.tst == "ztst")) {
      values$x0 <- seq(-4, 4, length = 100)
      values$x0 <- append(isolate(values$x0), values$stats, which(order(c(values$stats, isolate(values$x0))) == 1) - 1)
      values$y0 <- dnorm(isolate(values$x0))
      values$yendr <- dnorm(values$crit_val)
      values$yendl <- dnorm(-values$crit_val)
      values$xpoint <- qnorm(p = 1 - values$newalpha)
      values$xpoint2 <- qnorm(p = values$newalpha)
      if (input$HT_plts == "HT_pval") {
        if (input$type == "prop") {
          values$x0 <- sqrt((values$tsthat * (1 - values$tsthat)) / length(data()[, input$svar])) * isolate(values$x0) + input$nullhypo
        } else {
          values$x0 <- sqrt(input$sigma.squared / length(data()[, input$svar])) * isolate(values$x0) + input$nullhypo
        }
        values$statsP <- values$tsthat
      }
    } else if (input$type == "mu" && input$dif.tst == "ttst") {
      values$x0 <- seq(-4, 4, length = 100)
      values$x0 <- append(isolate(values$x0), values$stats, which(order(c(values$stats, isolate(values$x0))) == 1) - 1)
      values$y0 <- dt(isolate(values$x0), df = values$degf)
      values$yendr <- dt(values$crit_val, df = values$degf)
      values$yendl <- dt(-values$crit_val, df = values$degf)
      values$xpoint <- qt(p = 1 - values$newalpha, df = values$degf)
      values$xpoint2 <- qt(p = values$newalpha, df = values$degf)
      if (input$HT_plts == "HT_pval") {
        values$x0 <- (sd(data()[, input$svar]) / sqrt(length(data()[, input$svar]))) * isolate(values$x0) + input$nullhypo
        values$statsP <- values$xbar
      }
    } else if (input$type == "sigma") {
      values$x0 <- seq(0, qchisq(p = 0.999, df = values$degf), length = 100)
      values$x0 <- append(isolate(values$x0), values$stats, which(order(c(values$stats, isolate(values$x0))) == 1) - 1)
      values$y0 <- dchisq(values$x0, df = values$degf)
      if (input$althypo == "two.sided") {
        values$yendr <- dchisq(values$vcrit_val[2], df = values$degf)
        values$yendl <- dchisq(values$vcrit_val[1], df = values$degf)
      } else {
        values$yendr <- dchisq(values$crit_val, df = values$degf)
        values$yendl <- dchisq(values$crit_val, df = values$degf)
      }
      if (input$HT_plts == "HT_pval") { # browser()
        # values$x0<-(input$nullhypo*isolate(values$x0))/values$degf
        values$statsP <- values$tsthat
      }
    }
    # ind<-which.max(isolate(values$y0))
    dat <- data.frame(x = values$x0, y = values$y0)
    if (input$type == "sigma" & input$HT_plts == "HT_pval") {
      dat <- data.frame(x = ((input$nullhypo * isolate(values$x0)) / values$degf), y = values$y0)
    }
    ind <- which.max(dat$y)
    # if (input$type !="sigma") dat2=data.frame(x=values$x2,y1=values$y2)
    label <- paste0(
      sprintf("%5s", "TS:"), sprintf("%9s", attr(values$stats, "names")), " = ",
      sprintf("%.03f", values$stats)
    )
    # label=c(label,paste0(sprintf("%9s","df")," = ",sprintf("%.2f",values$degf)))
    
    if (values$pvalue >= 0.00001) {
      label <- c(label, paste0(sprintf("%9s", "pval"), " = ", sprintf("%.5f", values$pvalue)))
    } else {
      label <- c(label, paste0(sprintf("%9s", "pval"), " < 0.00001"))
    }
    label <- stringr::str_pad(label, 19, side = "right")
    label <- stringr::str_c(label, collapse = "\n")
    p2 <- ggplot(dat, aes_string(x = "x", y = "y")) +
      theme_bw() +
      geom_line(col = "white")
    if (input$HT_plts == "HT_CR") {
      if (values$alternative == "greater") {
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = values$vcrit_val[2], y = 0, yend = values$yendr), linetype = 2, color = "blue")
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = min(values$x0), y = values$yendr, yend = values$yendr), color = "green", arrow = arrow(length = unit(0.03, "npc")))
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = max(values$x0), y = values$yendr, yend = values$yendr), color = "red", arrow = arrow(length = unit(0.03, "npc")))
      } else if (values$alternative == "less") {
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = values$vcrit_val[1], y = 0, yend = values$yendl), linetype = 2, color = "blue")
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = max(values$x0), y = values$yendr, yend = values$yendr), color = "green", arrow = arrow(length = unit(0.03, "npc")))
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = min(values$x0), y = values$yendr, yend = values$yendr), color = "red", arrow = arrow(length = unit(0.03, "npc")))
      } else {
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = values$vcrit_val[2], y = 0, yend = values$yendr), linetype = 2, color = "blue")
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = values$vcrit_val[1], y = 0, yend = values$yendl), linetype = 2, color = "blue")
        p2 <- p2 + geom_segment(aes(x = min(values$x0), xend = max(values$x0), y = values$yendr, yend = values$yendr), color = "green")
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = max(values$x0), y = values$yendr, yend = values$yendr), color = "red", arrow = arrow(length = unit(0.03, "npc")))
        p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = min(values$x0), y = values$yendr, yend = values$yendr), color = "red", arrow = arrow(length = unit(0.03, "npc")))
      }
    } else {
      if (!is.null(input$confpval) && "pval" %in% input$confpval) {
        if (values$alternative == "two.sided") {
          if (input$type == "sigma") {
            lr <- (values$vpcrit[1] * input$nullhypo) / values$degf
            rr <- (values$vpcrit[2] * input$nullhypo) / values$degf
          } else {
            dif <- abs(values$statsP - input$nullhypo)
            rr <- input$nullhypo + dif
            lr <- input$nullhypo - dif
          }
          dat1 <- dat[dat$x <= lr, ]
          dat2 <- dat[dat$x >= rr, ]
          p2 <- p2 + geom_area(data = dat1, aes(x, y), fill = "red", alpha = 0.5)
          p2 <- p2 + geom_area(data = dat2, aes(x, y), fill = "red", alpha = 0.5)
        } else if (values$alternative == "less") {
          dat1 <- dat[dat$x <= values$statsP, ]
          p2 <- p2 + geom_area(data = dat1, aes(x, y), fill = "red", alpha = 0.5)
        } else {
          dat2 <- dat[dat$x >= values$statsP, ]
          p2 <- p2 + geom_area(data = dat2, aes(x, y), fill = "red", alpha = 0.5)
        }
      }
      if (!is.null(input$confpval) && "conf" %in% input$confpval) {
        dat1 <- data.frame(cbind(x = c(values$LCL, values$UCL), y = min(values$yendl, values$yendr) / 2))
        dat1$x[dat1$x == -Inf] <- min(dat$x)
        dat1$x[dat1$x == Inf] <- max(dat$x)
        p2 <- p2 + geom_area(data = dat1, aes(x, y), fill = "green", alpha = 0.5)
        p2 <- p2 + geom_area(data = dat1, aes(x, -y), fill = "green", alpha = 0.5)
        p2 <- p2 + geom_segment(aes(x = values$LCL, xend = values$LCL, y = -min(values$yendl, values$yendr) / 2, yend = min(values$yendl, values$yendr) / 2), linetype = 1, color = 3)
        p2 <- p2 + geom_segment(aes(x = values$UCL, xend = values$UCL, y = -min(values$yendl, values$yendr) / 2, yend = min(values$yendl, values$yendr) / 2), linetype = 1, color = 3)
      }
      values$nullhypo <- input$nullhypo
    }
    
    if (input$HT_plts == "HT_CR" || "pval" %in% input$confpval) {
      p2 <- p2 + geom_line() + annotate("text", x = dat$x[ind], y = dat$y[ind], label = "H[0]", parse = TRUE, size = 10, col = 2)
    }
    p2 <- p2 + annotate("text", x = values$nullhypo, y = 0, label = '"+"', parse = TRUE, size = 5, col = 2)
    p2 <- p2 + annotate("text", x = values$statsP, y = 0, label = '"*"', parse = TRUE, size = 10, col = 6)
    p2 <- p2 + geom_segment(aes(x = values$statsP, xend = values$statsP, y = 0, yend = min(values$yendl, values$yendr)), linetype = 4, color = 6)
    p2 <- p2 + annotate(geom = "label", x = Inf, y = Inf, label = label, vjust = 1.1, hjust = 1.1)
    p2 <- p2 + labs(title = values$method, x = paste0(values$statName, " statistic"), y = "Probability Density") + theme(plot.title = element_text(hjust = 0.5))
    p2
  })
  
  #### ONE + SAMPLE
  
  output$oneplusvar1 <- renderUI({
    selectizeInput("oneplusvar1", "Variable 1", choices = names(data()))
  })
  
  output$oneplusvar2 <- renderUI({
    selectizeInput("oneplusvar2", "Variable 2", choices = names(data())[-which(names(data()) == input$oneplusvar1)], multiple = FALSE)
  })
  
  output$oneplustype <- renderUI({
    if (is.null(ncol(data()))) {
      return()
    }
    if (is.numeric(data()[, input$oneplusvar1]) && is.numeric(data()[, input$oneplusvar2])) {
      selectInput("oneplustype", "Test for:", choices = c("Mean" = "mu"), selected = "Mean", width = "210px")
    } else if (is.factor(data()[, input$oneplusvar1]) && is.factor(data()[, input$oneplusvar2])) {
      selectInput("oneplustype", "Test for:", choices = c("Fisher Test of Independence" = "ftst_ind"), selected = "ftst_ind", width = "210px")
    } else if ((is.factor(data()[, input$oneplusvar1]) && is.numeric(data()[, input$oneplusvar2])) || (is.factor(data()[, input$oneplusvar2]) && is.numeric(data()[, input$oneplusvar1]))) {
      selectInput("oneplustype", "Test for:", choices = c("Anova" = "aov", "F Test to Compare Two Variances" = "ft2vr"), selected = "aov", width = "210px")
    }
  })
  
  ## Confidence Interval
  output$confintp <- renderUI({
    if (input$oneplustype == "aov" || input$oneplustype == "ftst_ind" || is.null(ncol(data()))) {
      return()
    }
    checkboxInput("confintp", "Confidence Interval", value = FALSE)
  })
  
  output$dependency <- renderUI({
    if (is.null(input$oneplustype)) {
      return()
    } else if (input$oneplustype == "mu") {
      radioButtons("dependency", "Samples are:", c("Independent" = FALSE, "Dependent" = TRUE), selected = FALSE, inline = TRUE)
    }
  })
  
  output$var.eq <- renderUI({
    if (is.null(input$oneplustype)) {
      return()
    } else if (input$oneplustype == "mu" && input$dependency == "FALSE") {
      radioButtons("var.eq", "Samples have:", c("Equal Variances" = TRUE, "Unequal Variances" = FALSE), selected = TRUE, inline = TRUE)
    }
  })
  
  ### Null hypothesis
  output$pnullhypo <- renderUI({
    if (is.null(input$oneplustype)) {
      return()
    } else if (input$oneplustype == "mu" && input$dependency == FALSE) {
      numericInput("pnullhypo", HTML("H<sub>0</sub>:<i> &mu; <sub>1</sub> - &mu; <sub>2</sub> ="), value = 0, width = "100px")
    } else if (input$oneplustype == "mu" && input$dependency == TRUE) {
      numericInput("pnullhypo", HTML("H<sub>0</sub>:<i> &mu; <sub>D</sub> ="), value = 0, width = "100px")
    } else if (input$oneplustype == "ft2vr") {
      numericInput("pnullhypo", HTML("H<sub>0</sub>:<i> &sigma;<sup>2</sup><sub>1</sub> / &sigma;<sup>2</sup><sub>2</sub> ="), value = 1, width = "100px")
    }
  })
  
  ### Alternate Hypothesis
  output$palthypo <- renderUI({
    if (is.null(input$oneplustype)) {
      return()
    } else if (input$oneplustype == "mu" && input$dependency == FALSE) {
      selectInput("palthypo", HTML("H<sub>a</sub>:<i> &mu; <sub>1</sub> - &mu; <sub>2</sub>"), choices = c("!=" = "two.sided", "<" = "less", ">" = "greater"), width = "100px")
    } else if (input$oneplustype == "mu" && input$dependency == TRUE) {
      selectInput("palthypo", HTML("H<sub>a</sub>:<i> &mu; <sub>D</sub>"), choices = c("!=" = "two.sided", "<" = "less", ">" = "greater"), width = "100px")
    } else if (input$oneplustype == "ft2vr") {
      selectInput("palthypo", HTML("H<sub>a</sub>:<i> &sigma;<sup>2</sup><sub>1</sub> / &sigma;<sup>2</sup><sub>2</sub>"), choices = c("!=" = "two.sided", "<" = "less", ">" = "greater"), width = "100px")
    }
  })
  
  output$oneplussamtst <- renderPrint({
    values$alpha <- 1 - input$conflevp
    if (is.null(input$oneplustype)) {
      return()
    } # || is.null(input$palthypo)) return()
    # Fisher test for independence
    else if (input$oneplustype == "ftst_ind") {
      fhtst <- fisher.test(table(data()[, input$oneplusvar1], data()[, input$oneplusvar2]), simulate.p.value = TRUE, hybrid = TRUE, conf.int = TRUE)
      fhtst # values$degf<-1;values$LCL <- fhtst$conf.int[1]; values$UCL <- fhtst$conf.int[2]; values$pvalue<-fhtst$p.value; values$stats<-fhtst$statistic;fhtst
    }
    # two-samples t-test (paired/unpaired) Default unpaired (independent)
    else if (input$oneplustype == "mu") {
      ttst <- t.test(data()[, input$oneplusvar1], data()[, input$oneplusvar2], paired = as.logical(input$dependency), alternative = input$palthypo, mu = input$pnullhypo, var.equal = as.logical(input$var.eq))
      values$degf <- ttst$parameter
      if (input$palthypo == "two.sided") {
        values$crit_val <- qt(input$conflevp + values$alpha / 2, df = values$degf, lower.tail = TRUE)
        values$pcrit_val <- qt(ttst$p.value / 2, df = values$degf, lower.tail = FALSE)
        values$vcrit_val <- c(-values$crit_val, values$crit_val)
      } else if (input$palthypo == "less") {
        values$crit_val <- qt(input$conflevp, df = values$degf, lower.tail = TRUE)
        values$pcrit_val <- qt(ttst$p.value, df = values$degf, lower.tail = FALSE)
        values$vcrit_val <- c(-values$crit_val, 0)
      } else if (input$palthypo == "greater") {
        values$crit_val <- qt(input$conflevp, df = values$degf, lower.tail = TRUE)
        values$pcrit_val <- qt(ttst$p.value, df = values$degf, lower.tail = FALSE)
        values$vcrit_val <- c(0, values$crit_val)
      }
      values$stderr <- ttst$stderr
      values$xbar <- ttst$estimate
      values$LCL <- ttst$conf.int[1]
      values$UCL <- ttst$conf.int[2]
      values$pvalue <- ttst$p.value
      values$stats <- ttst$statistic
      ttst
    }
    # # T-test numerical variables grouped by categorical variable with 2 levels
    # else if(input$oneplustype=="ttst"){
    #   if (is.factor(data[,input$oneplusvar1]) & length(levels(data[,input$oneplusvar1])) =2 | is.factor(data[,input$oneplusvar2]) & length(levels(data[,input$oneplusvar2]))=2 ){
    #     if(is.factor(data[,input$oneplusvar1])) {
    #       ttst<-t.test(subset(data,data[,input$oneplusvar1]==levels(data[,input$oneplusvar1])[1],select=data[,input$oneplusvar2]),subset(data,data[,input$oneplusvar1]==levels(data[,input$oneplusvar1])[2],select=data[,input$oneplusvar2]),
    #           paired=as.logical(input$dependency), alternative = input$palthypo,mu=input$pnullhypo, var.equal = as.logical(input$var.eq)); values$degf<-ttst$parameter;
    #     } else {
    #       ttst<-t.test(subset(data,data[,input$oneplusvar2]==levels(data[,input$oneplusvar2])[1],select=data[,input$oneplusvar1]),subset(data,data[,input$oneplusvar2]==levels(data[,input$oneplusvar2])[2],select=data[,input$oneplusvar1]),
    #           paired=as.logical(input$dependency), alternative = input$palthypo,mu=input$pnullhypo, var.equal = as.logical(input$var.eq)); values$degf<-ttst$parameter;
    #     }
    #
    #   }
    # }
    # F-test to test for homogeneity in variances.
    else if (input$oneplustype == "ft2vr") { # browser()
      if (is.factor(data()[, input$oneplusvar1]) & length(levels(data()[, input$oneplusvar1])) != 2 | is.factor(data()[, input$oneplusvar2]) & length(levels(data()[, input$oneplusvar2])) != 2) {
        return("One variable should be a factor with exactly 2 levels.")
      } else if (is.numeric(data()[, input$oneplusvar1])) {
        ftst <- var.test(data()[, input$oneplusvar1] ~ data()[, input$oneplusvar2], alternative = input$palthypo, var.equal = as.logical(input$var.eq), ratio = input$pnullhypo)
      } else {
        ftst <- var.test(data()[, input$oneplusvar2] ~ data()[, input$oneplusvar1], alternative = input$palthypo, var.equal = as.logical(input$var.eq), ratio = input$pnullhypo)
      }
      values$degf1 <- ftst$parameter[1]
      values$degf2 <- ftst$parameter[2]
      values$LCL <- ftst$conf.int[1]
      values$UCL <- ftst$conf.int[2]
      values$pvalue <- ftst$p.value
      values$stats <- ftst$statistic
      if (input$palthypo == "two.sided") {
        values$vcrit_val <- c(qf(p = values$alpha / 2, values$degf1, values$degf2), qf(p = 1 - values$alpha / 2, values$degf1, values$degf2))
      } else if (input$palthypo == "less") {
        values$vcrit_val <- c(qf(p = values$alpha, values$degf1, values$degf2), 0)
      } else if (input$palthypo == "greater") {
        values$vcrit_val <- c(0, qf(p = input$conflevp, values$degf1, values$degf2))
      }
      ftst
    }
    # Anova
    else if (input$oneplustype == "aov") { # browser()
      values$alternative <- "greater"
      if (is.numeric(data()[, input$oneplusvar1])) {
        aovtst <- aov(data()[, input$oneplusvar1] ~ data()[, input$oneplusvar2], data = data())
      } else {
        aovtst <- aov(data()[, input$oneplusvar2] ~ data()[, input$oneplusvar1], data = data())
      }
      # if (input$multcomp==TRUE) TukeyHSD(aovtst)
      values$degf1 <- summary(aovtst)[[1]][1, 1]
      values$degf2 <- summary(aovtst)[[1]][2, 1]
      values$pvalue <- summary(aovtst)[[1]][1, 5]
      values$stats <- summary(aovtst)[[1]][1, 4]
      if (values$alternative == "two.sided") {
        values$vcrit_val <- c(qf(p = values$alpha / 2, values$degf1, values$degf2), qf(p = 1 - values$alpha / 2, values$degf1, values$degf2))
      } else if (values$alternative == "less") {
        values$vcrit_val <- c(qf(p = values$alpha, values$degf1, values$degf2), 0)
      } else if (values$alternative == "greater") {
        values$vcrit_val <- c(0, qf(p = input$conflevp, values$degf1, values$degf2))
      }
      summary(aovtst)
    }
  })
  
  
  output$oneplussamtst.plt <- renderPlot({
    if (is.null(input$oneplustype) || input$oneplustype == "ftst_ind") {
      return()
    } #|| is.null(input$palthypo)
    values$alternative <- input$palthypo
    if (input$oneplustype == "aov") values$alternative <- "greater"
    if (values$alternative == "two.sided") values$newalpha <- values$alpha / 2 else values$newalpha <- values$alpha
    if (input$oneplustype == "mu") {
      values$x0 <- seq(-4, 4, length = 100)
      values$x0 <- append(isolate(values$x0), values$stats, which(order(c(values$stats, isolate(values$x0))) == 1) - 1)
      # if(values$stats>4) {
      # values$x0=c(isolate(values$x0),values$stats)
      # } else if(values$stats < -4) {
      # values$x0=c(values$stats,isolate(values$x0))
      # }
      values$y0 <- dt(values$x0, df = values$degf)
      values$yendr <- dt(values$crit_val, df = values$degf)
      values$yendl <- dt(-values$crit_val, df = values$degf)
      if (input$confintp == TRUE) {
        values$x0 <- values$stderr * isolate(values$x0) + input$pnullhypo
        values$stats <- values$xbar
      }
    } else if (input$oneplustype == "ft2vr") {
      values$x0 <- seq(qf(p = 0.0001, values$degf1, values$degf2), qf(p = 0.9999, values$degf1, values$degf2), length = 100)
      values$y0 <- df(values$x0, values$degf1, values$degf2)
      values$yendr <- df(values$vcrit_val[2], values$degf1, values$degf2)
      values$yendl <- df(values$vcrit_val[1], values$degf1, values$degf2)
    } else if (input$oneplustype == "aov") {
      values$x0 <- seq(qf(p = 0.0001, values$degf1, values$degf2), qf(p = 0.9999, values$degf1, values$degf2), length = 100)
      values$y0 <- df(values$x0, values$degf1, values$degf2)
      values$yendr <- df(values$vcrit_val[2], values$degf1, values$degf2)
      values$yendl <- df(values$vcrit_val[1], values$degf1, values$degf2)
    }
    ind <- which.max(isolate(values$y0))
    dat <- data.frame(x = values$x0, y = values$y0)
    # if (input$oneplustype != "ftst_ind") dat2=data.frame(x=values$x2,y1=values$y2)
    label <- paste0(
      sprintf("%9s", attr(values$stats, "names")), " = ",
      sprintf("%.03f", values$stats)
    )
    if (input$oneplustype == "ft2vr" || input$oneplustype == "aov") {
      label <- c(label, paste0("num df=", values$degf1, ", denom df=", values$degf2))
    } else {
      label <- c(label, paste0(sprintf("%9s", "df"), " = ", sprintf("%.2f", values$degf)))
    }
    if (values$pvalue >= 0.00001) {
      label <- c(label, paste0(sprintf("%9s", "p"), " = ", sprintf("%.5f", values$pvalue)))
    } else {
      label <- c(label, paste0(sprintf("%9s", "p"), " < 0.00001"))
    }
    label <- stringr::str_pad(label, 19, side = "right")
    label <- stringr::str_c(label, collapse = "\n")
    # if (input$oneplustype=="aov" | input$oneplustype=="ftst_ind" ) values$alternative <-"greater"
    
    p2 <- ggplot(dat, aes_string(x = "x", y = "y")) +
      geom_line() +
      theme_bw()
    if (values$alternative == "greater") {
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = values$vcrit_val[2], y = 0, yend = values$yendr), linetype = 2, color = "blue")
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = min(values$x0), y = values$yendr, yend = values$yendr), color = "green", arrow = arrow(length = unit(0.03, "npc")))
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = max(values$x0), y = values$yendr, yend = values$yendr), color = "red", arrow = arrow(length = unit(0.03, "npc")))
    } else if (values$alternative == "less") {
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = values$vcrit_val[1], y = 0, yend = values$yendl), linetype = 2, color = "blue")
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = max(values$x0), y = values$yendr, yend = values$yendr), color = "green", arrow = arrow(length = unit(0.03, "npc")))
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = min(values$x0), y = values$yendr, yend = values$yendr), color = "red", arrow = arrow(length = unit(0.03, "npc")))
    } else {
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = values$vcrit_val[2], y = 0, yend = values$yendr), linetype = 2, color = "blue")
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = values$vcrit_val[1], y = 0, yend = values$yendl), linetype = 2, color = "blue")
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = values$vcrit_val[2], y = values$yendr, yend = values$yendr), color = "green")
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[2], xend = max(values$x0), y = values$yendr, yend = values$yendr), color = "red", arrow = arrow(length = unit(0.03, "npc")))
      p2 <- p2 + geom_segment(aes(x = values$vcrit_val[1], xend = min(values$x0), y = values$yendr, yend = values$yendr), color = "red", arrow = arrow(length = unit(0.03, "npc")))
    }
    p2 <- p2 + annotate("text", x = values$x0[ind], y = values$y0[ind], label = "H[0]", parse = TRUE, size = 10, col = 2)
    if (!is.null(input$confintp) && input$confintp == TRUE) {
      p2 <- p2 + geom_vline(xintercept = c(values$LCL, values$UCL))
    }
    # if (input$oneplustype=="mu" & input$confintp==TRUE){
    #   p2<-ggplot(dat,aes_string(x="x",y="y"))+geom_line()+theme_bw()
    #   p2<-p2+geom_segment(aes(x=values$tsthat,xend=values$tsthat,y=0, yend=values$yendl),linetype=2,color="blue")
    #   if (values$alternative=="two.sided") {
    #     dif<- abs(values$stats-input$nullhypo); rr<- input$nullhypo+dif; lr<- input$nullhypo-dif
    #     dat1 <- dat[values$x0 <= lr,]; dat2 <- dat[values$x0 >=rr,]
    #     p2<-p2+geom_area(data=dat1,aes(x,y),fill="red",alpha=0.5)
    #     p2<-p2+geom_area(data=dat2,aes(x,y),fill="red",alpha=0.5)
    #   } else if (values$alternative=="less") {
    #     dat <- dat[values$x0 <= values$stats,]
    #     p2<-p2+geom_area(data=dat,aes(x,y),fill="red",alpha=0.5)
    #  } else {
    #    dat <- dat[values$x0 >= values$stats,]
    #    p2<-p2+geom_area(data=dat,aes(x,y),fill="red",alpha=0.5)
    #  }
    #   p2 <- p2 +geom_vline(xintercept=c(values$LCL,values$UCL))
    #   p2<- p2+annotate('text',x=values$x0[ind],y=values$y0[ind],label="H[0]",parse=TRUE,size=10,col=2)
    # }
    # if(values$alternative!="less")  p2<-p2+geom_area(data=dat1,aes(values$x1,values$y1),fill="red",alpha=0.5)
    # if(values$alternative!="greater")  p2<-p2+ geom_area(data=dat2,aes(values$x2,values$y2),fill="red",alpha=0.5)
    if (abs(values$stats) > 4) {
      hjust <- 1
    } else if (values$stats > 0) {
      hjust <- -0.1
    } else {
      hjust <- 0.1
    }
    
    p2 <- p2 + geom_segment(aes(x = values$stats, y = 0, xend = values$stats, yend = max(values$y0)), color = "blue")
    p2 <- p2 + annotate(geom = "label", x = Inf, y = Inf, label = label, vjust = 1.1, hjust = 1.1)
    # geom_text(x=xpoint2,y=0.38,label=label)+
    # p2 <-p2+ annotate(geom="text",x=ifelse(values$alternative=="less",values$xpoint2,values$xpoint),y=0,
    # label=paste0("p < ",values$alpha),vjust=1.5,color="red")
    p2 <- p2 + labs(title = values$method, x = paste0(values$statName, " statistic"), y = "Probability Density") + theme(plot.title = element_text(hjust = 0.5))
    p2
  })
})