# 2022 by The Sage (elise.r.d.lesage@gmail.com)
# Data analysis utilities

## PropMoreLessEqual  ####
# gives the proportion of the second columns that is greater, equal, and smaller than the first.
# useful as an effect size measure in Wilcoxon signed rank tests.
PropMoreLessEqual <- function(FirstCol, SecondCol){
  PropMore <- sum(ifelse(SecondCol>FirstCol, 1, 0), na.rm=TRUE)/length(!is.na(ot_summary_wide_acc$Day_Block_1_3))
  PropEqual <- sum(ifelse(SecondCol==FirstCol, 1, 0), na.rm=TRUE)/length(!is.na(ot_summary_wide_acc$Day_Block_1_3))
  PropLess <- sum(ifelse(SecondCol<FirstCol, 1, 0), na.rm=TRUE)/length(!is.na(ot_summary_wide_acc$Day_Block_1_3))
  output <- cbind(PropMore, PropEqual, PropLess)
  cat(sprintf("More: %0.2f | Equal: %0.2f | Less: %0.2f", PropMore, PropEqual, PropLess))
  return(output)
}