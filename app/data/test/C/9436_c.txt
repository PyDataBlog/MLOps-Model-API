#ifndef WIDGETSKU_H
#define WIDGETSKU_H

#include <TaoApiCpp/TaoDomain.h>
#include <QString>


/**
 * @brief 组件sku信息
 * 
 * @author sd44 <sd44sdd44@yeah.net>
 */
class WidgetSku : public TaoDomain
{

public:
 virtual ~WidgetSku() { }

  QString getPrice() const;
  void setPrice (QString price);
  QString getProps() const;
  void setProps (QString props);
  qlonglong getQuantity() const;
  void setQuantity (qlonglong quantity);
  qlonglong getSkuId() const;
  void setSkuId (qlonglong skuId);
  
  virtual void parseResponse();

private:
/**
 * @brief sku的价格，对应Sku的price字段
 **/
  QString price;

/**
 * @brief sku的属性串，根据pid的大小从小到大排列，前后都加";"。类型Sku的properties字段
 **/
  QString props;

/**
 * @brief sku的库存数量，对应Sku的quantity字段
 **/
  qlonglong quantity;

/**
 * @brief sku的id，对应Sku的sku_id字段
 **/
  qlonglong skuId;

};

#endif  /* WIDGETSKU_H */
