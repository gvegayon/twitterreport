// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cpp_tw_gender
IntegerVector cpp_tw_gender(CharacterVector x, CharacterVector male, CharacterVector female);
RcppExport SEXP twitterreport_cpp_tw_gender(SEXP xSEXP, SEXP maleSEXP, SEXP femaleSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type male(maleSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type female(femaleSEXP);
    __result = Rcpp::wrap(cpp_tw_gender(x, male, female));
    return __result;
END_RCPP
}
// cpp_tw_sentiment
NumericVector cpp_tw_sentiment(List x, CharacterVector neg, CharacterVector pos, NumericVector neg_score, NumericVector pos_score, CharacterVector neu, NumericVector neu_score);
RcppExport SEXP twitterreport_cpp_tw_sentiment(SEXP xSEXP, SEXP negSEXP, SEXP posSEXP, SEXP neg_scoreSEXP, SEXP pos_scoreSEXP, SEXP neuSEXP, SEXP neu_scoreSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type neg(negSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type pos(posSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type neg_score(neg_scoreSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pos_score(pos_scoreSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type neu(neuSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type neu_score(neu_scoreSEXP);
    __result = Rcpp::wrap(cpp_tw_sentiment(x, neg, pos, neg_score, pos_score, neu, neu_score));
    return __result;
END_RCPP
}
