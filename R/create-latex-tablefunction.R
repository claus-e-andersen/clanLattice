create.latex.table <-
  function(df, pretty.func = function(x, col.num = 1)
  {
    x
  }
  , NL = "\\\\", col.sign = "&")
  {
    # Library: clan
    # Created: March 22, 2002
    # Revised: March 22, 2002
    # Revised: April 13, 2005 Now, the NL-character is a parameter.
    # Revised: July 10, 2005 Now, the pretty.func also works on the last coloumn and its knows about the coloumn
    #          being handled.
    # Revised: July 19, 2005 updated sample calls
    # Revised: July 28, 2006 added the col.sign argument
    # Revised: August 7, 2006 Fixed bug such that the function can handle data frames with only one column
    # Name   : Claus E. Andersen
    # Task   : Generate a table for Latex (with &'s and \\-line endings).
    #          A function pretty.func can be supplied in the call.
    #          For example:
    #          ca <- function(x,col){y<-x;  if(class(x)=="numeric"){y <- signif(x,2)}; y}
    #          ca <- function(x,col){y<-x;  if(class(x)=="numeric"){ if(col==4) y <- round(x,1)  else  y <- round(x,4)};  y}
    #          ca <- function(x,col){y<-x;  if(class(x)=="numeric"){if(is.element(col,c(1,2))) y <- round.ca(x,0,scientific=c(-9,9)); if(is.element(col,c(3)))
    ##### y <- round.ca(x,2)  };  y}
    #          create.latex.table(data.frame(x=1:10,y=rnorm(10)*1e6,rep("test",10),yy=10000*rnorm(10)),ca)
    if(!is.null(df)) {
      N.cols <- ncol(df)
      N.rows <- nrow(df)
      ca <- pretty.func
      # Prepare a command that can assemble the columns into a new dataframe:
      # data.frame(org.name1 = x1, org.name2 = x2 etc. )
      # print(N.cols)
      # print(N.rows)
      new.line <- as.character("NL")
      df2 <- NULL
      if(T) {
        text <- "data.frame("
        if(N.cols > 1) {
          # More than one column
          for(i in 1:(N.cols - 1)) {
            text <- paste(text, "x", i, 
                          "=as.character(ca(df$", names(
                            df)[i], ",", i, ")), y", i,
                          "=rep(col.sign,", N.rows,
                          "),", sep = "")
          }
          # Special treatment of last element (no comma):
          i <- N.cols
          text <- paste(text, "x", i, 
                        "=as.character(ca(df$", names(df)[
                          i], ",", i, ")), y", i, "=rep(", 
                        new.line, ",", N.rows, "))", sep = "")
        }
        else {
          # Only one column
          text <- paste(text, "x", 1, 
                        "=as.character(ca(df$", names(df)[
                          1], ",", 1, ")), y", 1, "=rep(", 
                        new.line, ",", N.rows, "))", sep = "")
        }
        # Finally evaluate the command:
        # print(text)
        df2 <- eval(parse(text = text))
      }
      # Create line with labels:
      df3 <- NULL
      if(T) {
        text <- "data.frame("
        if(N.cols > 1) {
          # More than one column
          for(i in 1:(N.cols - 1)) {
            text <- paste(text, "x", i, 
                          "=as.character( '", names(
                            df)[i], "'), y", i, 
                          "=col.sign,", sep = "")
          }
          # Special treatment of last element (no comma):
          i <- N.cols
          text <- paste(text, "x", i, "=as.character( '",
                        names(df)[i], "'), y", i, "=", 
                        new.line, ")", sep = "")
        }
        else {
          # Only one column
          text <- paste(text, "x", 1, "=as.character( '",
                        names(df)[1], "'), y", 1, "=", 
                        new.line, ")", sep = "")
        }
        # Finally evaluate the command:
        # print(text)
        df3 <- eval(parse(text = text))
      }
      # Finally return the joined table:
      rr <- rbind(df3, df2)
    }
    else {
      # df was NULL
      rr <- NULL
    }
    rr
  }
