<?php
namespace BlackBoxCode\Pando\TaxBundle\Model;

use Doctrine\Common\Collections\ArrayCollection;
use Doctrine\ORM\Mapping as ORM;

trait RegionTrait
{
    /**
     * @var ArrayCollection<RegionTaxCategoryRateInterface>
     *
     * @ORM\OneToMany(targetEntity="RegionTaxCategoryRate", mappedBy="region")
     */
    private $taxCategoryRates;


    /**
     * {@inheritdoc}
     */
    public function getTaxCategoryRates()
    {
        if (is_null($this->taxCategoryRates)) $this->taxCategoryRates = new ArrayCollection();

        return $this->taxCategoryRates;
    }

    /**
     * {@inheritdoc}
     */
    public function addTaxCategoryRate(RegionTaxCategoryRateInterface $taxCategoryRate)
    {
        if (is_null($this->taxCategoryRates)) $this->taxCategoryRates = new ArrayCollection();
        $this->taxCategoryRates->add($taxCategoryRate);

        return $this;
    }

    /**
     * {@inheritdoc}
     */
    public function removeTaxCategoryRate(RegionTaxCategoryRateInterface $taxCategoryRate)
    {
        if (is_null($this->taxCategoryRates)) $this->taxCategoryRates = new ArrayCollection();
        $this->taxCategoryRates->removeElement($taxCategoryRate);
    }
}
