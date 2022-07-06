<?php
namespace Kondrat\DesignPatterns\Factory\AbstractFactory\Pizza;

class PepperoniPizza extends  AbstractPizza
{
    public function prepare()
    {
        $this->dough = $this->ingredientFactory->createDough();
        $this->sauce = $this->ingredientFactory->createSauce();
        $this->pepperoni = $this->ingredientFactory->createPepperoni();

        $res = "Preparing {$this->getName()} pizza...\n";

        return $res . parent::__toString($this);
    }
}
