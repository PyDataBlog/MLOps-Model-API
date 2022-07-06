#include <TaoApiCpp/response/SimbaInsightCatsforecastGetResponse.h>

QList<INCategoryTop> SimbaInsightCatsforecastGetResponse::getInCategoryTops() const {
  return inCategoryTops;
}
void SimbaInsightCatsforecastGetResponse::setInCategoryTops (QList<INCategoryTop> inCategoryTops) {
  this->inCategoryTops = inCategoryTops;
}

void SimbaInsightCatsforecastGetResponse::parseNormalResponse() {
  parseError();
  if (responseParser->hasName("in_category_tops")) {
  QList<Parser *> listParser = responseParser->getListObjectParser("in_category_tops");
  Parser *parser;
  foreach (parser, listParser) {
    INCategoryTop tmp;
    tmp.setParser(parser);
    tmp.parseResponse();
    inCategoryTops.append(tmp);
  }
  }

}

