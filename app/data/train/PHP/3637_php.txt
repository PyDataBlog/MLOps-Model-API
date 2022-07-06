<?php
namespace BlackBoxCode\Pando\ProductSaleBundle\Model;

use BlackBoxCode\Pando\BaseBundle\Model\IdInterface;

interface ProductVariantInventoryInterface extends IdInterface
{
    /**
     * @return integer
     */
    public function getAmount();

    /**
     * @param integer $amount
     * @return $this
     */
    public function setAmount($amount);

    /**
     * @return ProductVariantInterface
     */
    public function getProductVariant();

    /**
     * @param ProductVariantInterface $productVariant
     * @return $this
     */
    public function setProductVariant(ProductVariantInterface $productVariant);
}
