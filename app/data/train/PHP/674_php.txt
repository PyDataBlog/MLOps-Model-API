<?php
/**
 * H4PH - HTML4 PHP Helper 
 * @link https://github.com/Coft/H4PH
 */

class Td
{
    /**
     * contains all setted attributes 
     * @var array 
     */
    private $attrs = array();
    /**
     * keeps content 
     * @var string 
     */
    private $content = null;
    /**
     * holds markup name 
     * @var string 
     */
    private static $markupName = 'td';
    /**
     * tells is this markup can handle content 
     * @var bool 
     */
    private static $isContenerable = true;

    /**
     * __construct() - can add content to tag
     * @param  null|string $content
     * @return  Td 
     */
    public function __construct($content = null) 
    {
        $argumentsNumber = func_num_args();
        for ($i = 0; $i < $argumentsNumber; $i++) {
            $this->addContent(func_get_arg($i));
        }
    }

    /**
     * renders tag and it content to string
     * @param  null|string $content
     * @return  Td 
     */
    public static function getInstance($content = null) 
    {
        $markupTag = new Td;
        $argumentsNumber = func_num_args();
        for ($i = 0; $i < $argumentsNumber; $i++) {
            $markupTag->addContent(func_get_arg($i));
        }
        return $markupTag;
    }

    /**
     * adds content to tag
     * @param  null|string $content
     * @return  Td 
     */
    public function addContent($content = null) 
    {
        $this->content .= (string) $content;
        return $this;
    }

    /**
     * renders tag and it content to string
     * @return string
     */
    public function __toString() 
    {
        $parsedAttrs = '';
        foreach ($this->attrs as $attrName => $attrValue) {
            $parsedAttrs .= ' '.$attrName.'=\''.$attrValue.'\'';
        }
        return '<'.self::$markupName.$parsedAttrs.'>'.$this->content.'</'.self::$markupName.'>';
    }

    /**
     * sets abbr attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function abbr($value = null) 
    {
        $this->attrs['abbr'] = $value;
        return $this;
    }

    /**
     * sets align attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function align($value = null) 
    {
        $this->attrs['align'] = $value;
        return $this;
    }

    /**
     * sets axis attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function axis($value = null) 
    {
        $this->attrs['axis'] = $value;
        return $this;
    }

    /**
     * sets char attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function char($value = null) 
    {
        $this->attrs['char'] = $value;
        return $this;
    }

    /**
     * sets charoff attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function charoff($value = null) 
    {
        $this->attrs['charoff'] = $value;
        return $this;
    }

    /**
     * sets colspan attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function colspan($value = null) 
    {
        $this->attrs['colspan'] = $value;
        return $this;
    }

    /**
     * sets headers attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function headers($value = null) 
    {
        $this->attrs['headers'] = $value;
        return $this;
    }

    /**
     * sets rowspan attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function rowspan($value = null) 
    {
        $this->attrs['rowspan'] = $value;
        return $this;
    }

    /**
     * sets scope attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function scope($value = null) 
    {
        $this->attrs['scope'] = $value;
        return $this;
    }

    /**
     * sets valign attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function valign($value = null) 
    {
        $this->attrs['valign'] = $value;
        return $this;
    }

    /**
     * sets classes attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function classes($value = null) 
    {
        $this->attrs['class'] = $value;
        return $this;
    }

    /**
     * sets dir attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function dir($value = null) 
    {
        $this->attrs['dir'] = $value;
        return $this;
    }

    /**
     * sets id attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function id($value = null) 
    {
        $this->attrs['id'] = $value;
        return $this;
    }

    /**
     * sets lang attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function lang($value = null) 
    {
        $this->attrs['lang'] = $value;
        return $this;
    }

    /**
     * sets style attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function style($value = null) 
    {
        $this->attrs['style'] = $value;
        return $this;
    }

    /**
     * sets title attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function title($value = null) 
    {
        $this->attrs['title'] = $value;
        return $this;
    }

    /**
     * sets xmlLang attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function xmlLang($value = null) 
    {
        $this->attrs['xml:lang'] = $value;
        return $this;
    }

    /**
     * sets onclick attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onclick($value = null) 
    {
        $this->attrs['onclick'] = $value;
        return $this;
    }

    /**
     * sets ondblclick attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function ondblclick($value = null) 
    {
        $this->attrs['ondblclick'] = $value;
        return $this;
    }

    /**
     * sets onmousedown attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onmousedown($value = null) 
    {
        $this->attrs['onmousedown'] = $value;
        return $this;
    }

    /**
     * sets onmousemove attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onmousemove($value = null) 
    {
        $this->attrs['onmousemove'] = $value;
        return $this;
    }

    /**
     * sets onmouseout attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onmouseout($value = null) 
    {
        $this->attrs['onmouseout'] = $value;
        return $this;
    }

    /**
     * sets onmouseover attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onmouseover($value = null) 
    {
        $this->attrs['onmouseover'] = $value;
        return $this;
    }

    /**
     * sets onmouseup attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onmouseup($value = null) 
    {
        $this->attrs['onmouseup'] = $value;
        return $this;
    }

    /**
     * sets onkeydown attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onkeydown($value = null) 
    {
        $this->attrs['onkeydown'] = $value;
        return $this;
    }

    /**
     * sets onkeypress attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onkeypress($value = null) 
    {
        $this->attrs['onkeypress'] = $value;
        return $this;
    }

    /**
     * sets onkeyup attribute
     * @param  null|string $value
     * @return  Td 
     */
    public function onkeyup($value = null) 
    {
        $this->attrs['onkeyup'] = $value;
        return $this;
    }
}
 ?>