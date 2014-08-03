#' @title Stack a dataframe such that it is ideal for Lattice graphics plotting
#' @description Reorganize dataframe in a way needed for trellis plotting using the lattice graphical
#' package. This is more advanced function than make.groups. For example: You conduct two experiments and 
#' fit a triple exponental curve to each experiment. In the dataframe with fitting results, you will 
#' typically end up with three columns each holding the results of the three individual time constants. 
#' This function can reorginize the dataframe such that all time constants are stacked on top of each 
#' other (in one single column e.g. called t.const). An additional column (e.g. called components = "slow", 
#' "medium", "fast") tells what is what. Initial dataframe:
#'    ID   t1   t2     t3
#'    A   0.1  5.0  100.4
#'    B   0.2  5.1  102.0
#'
#' Result after the call stack.for.trellis(df,c("t1","t2","t3"),col.name="t.const",col.name.which="component")
#'   ID t.const component
#'    A     0.1      fast
#'    B     0.2      fast
#'    A     5.0    medium
#'    B     5.1    medium
#'    A   100.4      slow
#'    B   102.0      slow
#'
#' It is now possible to mate a dotplot as follows dotplot(ID~t.const|component,data=df.stacked).
#' @usage
#'    df.testdata <- data.frame(ID=c("A","B"),t1=c(0.1,0.2),t2=c(5.0,5.1),t3=c(100.4,102.0),junk=c(1,1),junk2=c(3,3))
#'    stack.for.trellis(df.testdata,c("t1","t2","t3"),col.name="t.const",col.name.which="component",remove.stacked=T,remove.others=T,remove.never="ID")
#'    stack.for.trellis(df.testdata,c("t1","t2","t3"),col.name="t.const",col.name.which="component",remove.stacked=T,remove.others=T)
#'    stack.for.trellis(df.testdata,c("t1","t2","t3"),col.name="t.const",col.name.which="component")
#' @name stack.for.trellis
#' @author Claus E. Andersen
#' @return A reorgainzed data frame
#' @param df is the input dataframe
#' @param vars is a vector of names of the variables in df to be stacked
#' @param col.name is the name of the column in the new (stacked) data frame with the "real" data
#' @param col.name.which is the name of the column in the new (stacked) data frame where the identifier 
#' is put. By default this is simply called "which".
#' @param labels are names of the different identifiers.
#' @param remove.stacked: If TRUE, the  columns that were used for the stacking willbe removed in the output.
#' @param remove.others: If TRUE columns that were not used for the stacking will be removed (e.g. junk1 
#' and junk2 in the example above).
#' @param remove.never: Column names that should never be removed regardless of the settings of remove.stacked
#' or remove.others.
#' @export stack.for.trellis
stack.for.trellis <- function(df, vars, col.name = "data", col.name.which = "which", labels = vars,
                              remove.stacked = FALSE, remove.others = FALSE, remove.never = "")
{
  # Created: February 14, 2003
  # Revised: April 26, 2003
  # Name:    Claus E. Andersen
  # Library: clan
  # Reorganize dataframe in a way that is needed for trellis. This is more advanced function than make.groups.
  # For example: You conduct two experiments and fit a triple exponental curve to each experiment. In the dataframe
  # with fitting results, you will typically end up with three columns each holding the results of the three
  # individual time constants. This function can reorginize the dataframe such that all time constants are
  # stacked on top of each other (in one single column e.g. called t.const). An additional column (e.g.
  # called components = "slow", "medium", "fast") tells what is what.
  # Initial dataframe:
  #    ID   t1   t2     t3
  #    A   0.1  5.0  100.4
  #    B   0.2  5.1  102.0
  #
  # Result after the call stack.for.trellis(df,c("t1","t2","t3"),col.name="t.const",col.name.which="component")
  #   ID t.const component
  #    A     0.1      fast
  #    B     0.2      fast
  #    A     5.0    medium
  #    B     5.1    medium
  #    A   100.4      slow
  #    B   102.0      slow
  #
  # It is now possible to mate a dotplot as follows dotplot(ID~t.const|component,data=df.stacked).
  # Input:
  # df   = data frame
  # vars = a vector of names of the variables in df to be stacked
  # col.name       = name of the column in the new (stacked) data frame with the "real" data
  # col.name.which = name of the column in the new (stacked) data frame where the identifier is put
  # labels         = names of the different identifiers
  # remove.stacked = columns that are used for the stacking can be removed
  # remove.others  = columns that are not used for the stacking can be removed (e.g. junk1 and junk2 in the example below)
  # Sample calls:
  #   df.testdata <- data.frame(ID=c("A","B"),t1=c(0.1,0.2),t2=c(5.0,5.1),t3=c(100.4,102.0),junk=c(1,1),junk2=c(3,3))
  #   stack.for.trellis(df.testdata,c("t1","t2","t3"),col.name="t.const",col.name.which="component",remove.stacked=T,remove.others=T,remove.never="ID")
  #   stack.for.trellis(df.testdata,c("t1","t2","t3"),col.name="t.const",col.name.which="component",remove.stacked=T,remove.others=T)
  #   stack.for.trellis(df.testdata,c("t1","t2","t3"),col.name="t.const",col.name.which="component")
  N.vars <- length(vars)
  N.rows <- nrow(df)
  col <- df[, vars[1]]
  col.which <- rep(labels[1], N.rows)
  if(N.vars > 1) {
    for(i in 2:N.vars) {
      col <- c(col, df[, vars[i]])
      col.which <- c(col.which, rep(labels[i], N.rows))
    }
  }
  for(i in 1:N.vars) {
    if(i == 1)
      df.stack <- df
    else df.stack <- rbind(df.stack, df)
  }
  nams <- names(df)
  txt <- paste("df.stack$", col.name, "<- col", sep = "")
  eval(parse(text = txt))
  txt <- paste("df.stack$", col.name.which, "<- col.which", sep = "")
  eval(parse(text = txt))
  for(i in nams) {
    keep <- T
    if(is.element(i, vars) & remove.stacked) {
      keep <- F
    }
    if(!is.element(i, vars) & remove.others) {
      keep <- F
    }
    if(is.element(i, c(col.name, col.name.which, remove.never))) 
    {
      keep <- T
    }
    # Never remove these variables
    if(!keep) {
      # Remove this variable
      j <- (1:ncol(df.stack))[i == names(df.stack)]
      df.stack <- df.stack[,  - j]
    }
  }
  return(df.stack)
}
