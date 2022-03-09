# 2022 by The Sage (elise.r.d.lesage@gmail.com)
# Various data cleaning util functions

## PropMoreLessEqual  ####
# gives the proportion of the second columns that is greater, equal, and smaller than the first.
# useful as an effect size measure in Wilcoxon signed rank tests.
function PropMoreLessEqual(FirstCol, SecondCol){
  PropMore <- sum(ifelse(SecondCol>FirstCol, 1, 0))
  PropEqual <- sum(ifelse(SecondCol==FirstCol, 1, 0))
  PropLess <- sum(ifelse(SecondCol<FirstCol, 1, 0))
  disp(sprintf("More: %0.2f | Equal: %0.2f | Less: %0.2f", PropMore, PropEqual, PropLess))
  return(list(PropMore, PropEqual, PropLess))
}