#' @title Make a Lattice graph of text and data
#' @description This function can print text or dataframes on a separate graphsheet or on 
#' an excisting graph. The main idea behind the function is to add experimental or 
#' analytical details (=text) on separate 'graphs' in between the real graphs. 
#' When everything is exported to pdf or PowerPoint we both have explanations and graphs 
#' in the same document. At the same time, we can easily read the text in the script. 
#' Hence this function should facilitate improved documentation. This is kind of a 
#' simple way of literate programming.
#' @usage
#' txtplot("hello world")
#' @name txtplot
#' @author Claus E. Andersen
#' @return The current position is returned. This can be input on subsequent calls as start.pos.
#' @param txt is the text or the dataframe to be printed
#' @param new: If true, txtplot opens a new graphsheet for graphical output
#' @param start.position is a list with the start (x,y) position and an optional lineno on the page. 
#' start.pos = list(0.5,0.5, 6) refers to the center of the page and that the current line number is 6. 
#' The line number is used for keeping track of when we need a new page. If start.pos==NULL, then we 
#' just use the defaults x.start.default and y.start.default.
#' @param maintain.start.pos: If false, then start each new line at x=x.start.default otherwise 
#' start each new line at the orginal x-start.pos
#' @param new.line: If true,a new line is added after txt. Alternatively, include a backslash-n in the text.
#' @param  page.size: number of lines before page break.
#' @param line.size: number of characters on a given line before line break (with or without hyphen)
#' @param line.size.extra: to promote that lines are broken where there is white space we allow for
#' a more relaxed break of lines. txtplot tries to break lines when there are more characters than 
#' given by line.size. However, it does not break before it reached a blank space - in so far that 
#' blank space occurs before the number of character is in the line is larger than: line.size + line.size.extra.
#' @param head: not implemented
#' @param foot: not implemented
#' @param evaluate: treat txt as commands, and execute these (has never been used).
#' @param adjust.table: add leading blanks using create.latex.table with the pretty.func argument
#' @param table.lines: a vector with line numbers where horizontal lines should be made. This only
#' applies to when txt is a data.frame.  999 is the last line. table.lines = TRUE is equivalent to
#' table.lines =c(1,2,999) table.lines = F gives no table lines. Note: The table lines are drawn 
#' using the lines-command and is therefore restricted to the x-y drawing panel (not the full 0-1 
#' by 0-1 coordinate system). In comparison, the text is drawn using the mtext-command. Set 
#' trace=TRUE and a red box will show where table lines can be drawn. If table lines are requested
# then txtplot moves the table into the 'line''-domain automatically.
#' @param table.line.pos: Fine control of where table lines should be placed.
#' @param col.sign: the sign between coloumns (used by create.latex.table) 
#' @param hyphen.sign: the sign used by txtplot when it breaks lines.
#' @param cex (printing formatting)
#' @param col (printing formatting)
#' @param font (printing formatting).
#' @param char.width: distance between the start of each character (the size of each character is set by cex).
#' @param line.height: height of each line
#' @param   box: list(coordinates, col, lwd, lty) or logical. The coordinates should be provided
#' in the same order as usr (i.e. as: c(x1,x2,y1,y2)).
#' @param char.groupA/B/C: Some characters do not have the same reference point as others, and we need to move these
#' somewhat. Each char.groupX is a list with three elements: a vector of characters, a number that specify the 
#' movements in the x and y directions (x.right and y.lift, respectively).
#' @param trace: debugging information
#' @param x.start.default: The default start x-position 
#' @param y.start.default: The default start y-position
#' @param vp: The viewport that will be created for new pages.
#' @export 
txtplot <- function(txt, new = TRUE, start.pos = NULL, maintain.start.pos = TRUE, new.line = FALSE, 
                    page.size = 20/cex, line.size = 75/cex - 5, line.size.extra = 5 + line.size * 0.03, 
                    head = "", foot = "", evaluate = FALSE, 
                    adjust.table = TRUE, table.lines = c(1, 2, 999), table.line.pos = list(start = 1, stop = 4, frac = 0.3), 
                    col.sign = " ", hyphen.sign = "-", 
                    cex = 0.8, col = 1, font = 1, font.family="Courier", char.width = 0.013 * cex, 
                    line.height = 0.05 * cex, 
                    box = NULL, 
                    char.groupA = list(chars = c("#", "~", "^", "[", "]", "@"), x.right = -0.005 * cex*0, y.lift = -0.015 * cex*0), 
                    char.groupB = list(chars = c("-"), x.right = 0., y.lift = 0.001 * cex), 
                    char.groupC = NULL, 
                    trace = FALSE, 
                    x.start.default=0, y.start.default=1,
                    vp = grid::viewport(x=grid::unit(0.025,"npc"),y=grid::unit(0.025,"npc"),width=grid::unit(0.95,"npc"),height=grid::unit(0.95,"npc"),just=c("left","bottom"),name="vp.txtplot"),
                    ...)
{ # This function can print text or dataframes on a separate graphsheet or on an excisting graph.
  # The main idea behind the function is to add experimental or analytical details (=text) on
  # separate 'graphs' in between the real graphs. When everything is exported to pdf or PowerPoint
  # we both have explanations and graphs in the same document. At the same time, 
  # we can easily read the text in the script. Hence this function should facilitate
  # improved documentation. This is kind of a simple way of literate programming. 
  
  # Created: July 27, 2006
  # Revised: July 28, 2006
  # Revised: July 29, 2006
  # Revised: July 30, 2006
  # Revised: July 31, 2006
  # Revised: October 30, 2011 (moved to R and updated)
  # Name: Claus E. Andersen
  # Contact : clan@risoe.dtu.dk
  
  # Arguments.
  #   txt :  the text to be plotted or a data.frame
  #   new: if true, txtplot opens a new graphsheet for graphical output
  #   start.pos: a list with the start (x,y) position and an optional lineno on the page. 
  #              start.pos = list(0.5,0.5, 6) refers to the center of the page and that 
  #              the current line number is 6. The line number is used for keeping track
  #              of when we need a new page. If start.pos==NULL, then we just use the defaults
  #              x.start.default and y.start.default.
  #   maintain.start.pos: If false, then start each new line at x=x.start.default otherwise 
  #                       start each new line at the orginal x-start.pos
  #   new.line: Add a new line after txt. Alternatively, include a \n-in the text.
  #   page.size: number of lines before page break.
  #   line.size: number of characters on a given line before line break (with or without hyphen)
  #   line.size.extra: to promote that lines are broken where there is white space we allow for
  #                     a more relaxed break of lines. txtplot tries to break lines when there are
  #                     more characters than given by line.size. However, it does not break before
  #                     it reached a blank space - in so far that blank space occurs before the
  #                     number of character is in the line is larger than: line.size + line.size.extra.
  #   head: not implemented
  #   foot: not implemented
  #   evaluate: treat txt as commands, and execute these (has never been used).
  #   adjust.table: add leading blanks using create.latex.table which the pretty.func argument
  #   table.lines: a vector with line numbers where horizontal lines should be made. This only
  #                applies to when txt is a data.frame.  999 is the last line.
  #                table.lines = T is equivalent to table.lines =c(1,2,999)
  #                table.lines = F gives no table lines.
  #                Note: The table lines are drawn using the lines-command and is therefore
  #                restricted to the x-y drawing panel (not the full 0-1 by 0-1 coordinate system).
  #                In comparison, the text is drawn using the mtext-command. Set trace=T and
  #                a red box will show where table lines can be drawn. If table lines are requested
  #                then txtplot moves the table into the 'line''-domain automatecially.
  #   table.line.pos: Fine control of where table lines should be placed.
  #   col.sign: the sign between coloumns (used by create.latex.table)
  #   hyphen.sign: the sign used by txtplot when it breaks lines.
  #   cex, col, font: use these values for the printing.
  #   char.width: distance between the start of each character (the size of each character is set by cex)
  #   line.height: height of each line
  #   box: list(coordinates, col, lwd, lty) or logical. The coordinates should be provided
  #        in the same order as usr (i.e. as: c(x1,x2,y1,y2)).
  #   char.groupA/B/C: Some characters do not have the same reference point as others, and we need to move these
  #                    somewhat. Each char.groupX is a list with three elements: a vector of characters, a
  #                    number that specify the movements in the x and y directions (x.right and y.lift, respectively).
  #   trace: debugging information
  #   x.start.default: The default start x-position 
  #   y.start.default: The default start y-position
  #   vp: The viewport that will be created for new pages.
  
  # Output: list of coordinates (can be used by subsequent calls to txtplot - see example below).
  
  # Side effect: Produces one or more pages of empty graphs with text or adds text to an existing graph.
  
  # Sample call:
  #    txt <- 
  #      "
  #      Hi there
  #      This is txtplot...
  #      Bye Bye.
  #      "
  #      txtplot(txt)
  # See txtplot.demo() for additionalk examples.
  
  # Repair any missing line number (= the third element in the start.pos list):
  if(!is.null(start.pos) && length(start.pos)==2) start.pos <- list(start.pos[[1]],start.pos[[2]], 1)
  
  if(new) {
    # Create a new (empty) graphsheet for trellis graphics
    #	Old S-plus:	print.trellis.plots(list(NULL))
    #		          print.trellis(list(NULL))
    grid::grid.newpage()
  }
  
  # Always make a viewport:
  grid::pushViewport(vp)
  
  usr <- par("usr")
  if(trace) {
    lattice::llines(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0), lwd = 2, col = 6)
    lattice::llines(c(usr[1], usr[1], usr[2], usr[2], usr[1]), c(usr[3],
                                                        usr[4], usr[4], usr[3], usr[3]), lwd = 2, col = 8)
  }
  if(!is.null(box)) {
    if(!is.list(box))
      box <- list(box)
    bc <- box[[1]]
    if(is.logical(bc)) {
      if(bc)
        bc <- c(usr[1], usr[2], usr[3], usr[4])
      else {
        bc <- c(NA, NA, NA, NA)
      }
    }
    if(is.null(box$col))
      box$col <- 1
    if(is.null(box$lwd))
      box$lwd <- 3
    if(is.null(box$lty))
      box$lty <- 4
    lattice::llines(c(bc[1], bc[1], bc[2], bc[2], bc[1]), c(bc[3], bc[4],
                                                   bc[4], bc[3], bc[3]), lwd = box$lwd, col = box$col,
           lty = box$lty)
  }
  if(is.null(table.lines) || is.na(table.lines[1])) {
    table.lines <- c(-999)
  }
  if(is.logical(table.lines)) {
    if(table.lines)
      table.lines <- c(1, 2, 999)
    else table.lines <- c(-99)
  }
  if(is.null(start.pos)) {
    x0 <- x.start.default  #WAS: 0.
    y0 <- y.start.default  #Was: 1.
    line.no <- 0
    tabline.no <- 0
  }
  else {
    x0 <- start.pos[[1]]
    y0 <- start.pos[[2]]
    line.no <- start.pos[[3]]
    tabline.no <- 0
  }
  # remember this (for table with many lines that should start from same x)
  x.start.pos <- x0
  if(!maintain.start.pos)
    x.start.pos <- 0.
  if(new.line) {
    x0 <- x.start.default  #WAS: 0.
    y0 <- y0 - line.height
  }
  dat.frame <- F
  if(trace) {
    lattice::ltext(0, 0, "0,0")
    lattice::ltext(0, 1, "0,1")
    lattice::ltext(1, 0, "1,0")
    lattice::ltext(1, 1, "1,1")
  }
  NN <- 0
  line.wanted <- F
  if(class(txt) == "data.frame") {
    dat.frame <- T
    tabline.no <- 0
    if(sum(table.lines) > 0)
      y0 <- min(y0, usr[4] - line.height)
    if(adjust.table) {
      #df  <- create.latex.table(txt,NL="",pretty.func = function(x, col.num = 1){if(is.numeric(x))signif(x,4)else x})
      df <- clanTools::create.latex.table(txt, NL = "", col.sign = 
                                 col.sign, ...)
      df <- clanTools::leading.blanks(df, " ")
    }
    txt <- ""
    for(i in 1:nrow(df)) {
      L <- ""
      for(j in 1:ncol(df)) {
        L <- paste(L, format(df[i, j], digits = 3,
                             justify = "left"))
      }
      # cols
      txt <- paste(txt, L, "\n")
    }
    # rows
    #lines(c(0,1),c(0.3,0.3),type="l")
    # Identify first line in table, and find it's length (i.e. the first \n-match)
    NN <- regexpr("[\n]", txt)
  }
  # data.frame
  if(class(txt) == "character") {
    ##line.no <- 0
    char.no <- 0
    for(i in 1:nchar(txt)) {
      ch <- substring(txt, i, i)
      char.no <- char.no + 1
      if(trace)
        print(paste("char no.=", i, ch))
      if((ch == "\n")) {
        char.no <- 1
        x0 <- x.start.pos
        y0 <- y0 - line.height
        ch = ""
        line.no <- line.no + 1
        tabline.no <- tabline.no + 1
        if(is.element(tabline.no, table.lines))
          line.wanted <- T
      }
      if((!(ch == "")) & (((ch == " ") & (char.no > 
                                            line.size)) | ((char.no > line.size + 
                                                              line.size.extra)))) {
        char.no <- 1
        if(!(ch == " "))
          text(x0, y0, hyphen.sign, adj = 0,
               cex = cex, col = col, font = font, family = font.family)
        if(ch == " ")
          ch <- ""
        x0 <- x.start.pos
        y0 <- y0 - line.height
        line.no <- line.no + 1
        tabline.no <- tabline.no + 1
        if(is.element(tabline.no, table.lines))
          line.wanted <- T
      }
      x.right <- 0.
      y.lift <- 0.
      if(!is.null(char.groupA) && is.element(ch, char.groupA$
                                               chars)) {
        x.right <- char.groupA$x.right
        y.lift <- char.groupA$y.lift
      }
      if(!is.null(char.groupB) && is.element(ch, char.groupB$
                                               chars)) {
        x.right <- char.groupB$x.right
        y.lift <- char.groupB$y.lift
      }
      if(!is.null(char.groupC) && is.element(ch, char.groupC$
                                               chars)) {
        x.right <- char.groupC$x.right
        y.lift <- char.groupC$y.lift
      }
      lattice::ltext(x0 + x.right, y0 + y.lift, substring(txt, i,
                                                 i), col = col, font = font, font.family = font.family, cex = cex)
      if(dat.frame & line.wanted) {
        xL.start <- x.start.pos + table.line.pos$
          start * char.width
        xL.stop <- x.start.pos + (NN - table.line.pos$
                                    stop) * char.width
        yL <- y0 + line.height * (1 + table.line.pos$
                                    frac)
        if(trace) {
          print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
          )
          print("usr = ")
          print(usr)
          print(paste("x start", xL.start))
          print(paste("x stop", xL.stop))
          print(paste("y ", yL))
        }
        # trace
        xL.start <- max(xL.start, 0)
        xL.stop <- min(xL.stop, 1)
        if(yL < usr[3] | yL > usr[4])
          yL <- NA
        lattice::llines(c(xL.start, xL.stop), c(yL, yL), type = 
                 "l", col = col)
        line.wanted <- F
      }
      if(!(ch == "")) x0 <- x0 + char.width
      
      if(line.no > page.size) {
        grid::grid.newpage()
        grid::pushViewport(vp)
        line.no <- 1
        y0 <- y.start.default # Was: 1.
      }
    }
    if(dat.frame & is.element(999, table.lines)) {
      xL.start <- x.start.pos + table.line.pos$start * char.width
      xL.stop <- x.start.pos + (NN - table.line.pos$stop) * char.width
      yL <- y0 + line.height * (0 + table.line.pos$frac)
      xL.start <- max(xL.start, 0)
      xL.stop <- min(xL.stop, 1)
      if(yL < usr[3] | yL > usr[4]) yL <- NA
      lattice::llines(c(xL.start, xL.stop), c(yL, yL), type = "l", col = col)
    }
  }
  # character
  if(trace) {
    print("The following text will now be evaluated.")
    print("---- Begin of text to be evaluated ----")
    print(txt)
    print("---- END of text to be evaluated ----")
  }
  if(evaluate) eval(parse(text = txt))
  grid::popViewport()
  invisible(list(x0, y0, line.no))
}# End txtplot
