<?php
// Esercizio: un numero che si dimezza sempre
header('Content-Type: text/plain');
ini_set('display_errors', true); // MAI in produzione!!!
ini_set('html_errors', 0);

/**
 * Questa classe continua a dividere
 * il suo stato interno per un valore dato.
 */
class invert
{
    /**
     * Il valore corrente
     * @var integer
     */
    public $val = 1;

    /**
     * Il divisore
     * @var integer
     */
    protected $_divisor = 3;

    /**
     * Esegue la divisione e restituisce il risultato
     * @return integer
     */
    public function div()
    {
        $this->val = $this->val / $this->_divisor;
        return $this->val;
    }

    /**
     * Imposta il valore del divisore
     * @param integer $divisor
     * @return void
     */
    public function setDivisor($divisor)
    {
        if ($divisor) {
            $this->_divisor = $divisor;
        } else {
            // Prendo dei provvedimenti
        }
    }
}


$sempreMeno = new invert;
while ($sempreMeno->val > 1e-5) {
    echo $sempreMeno->div(), PHP_EOL;
}
// Tento di impostare un nuovo divisore...
$sempreMeno->_divisor = 2;

// ...ma devo usare la funzione
$sempreMeno->setDivisor(2);
echo $sempreMeno->div(), PHP_EOL;
