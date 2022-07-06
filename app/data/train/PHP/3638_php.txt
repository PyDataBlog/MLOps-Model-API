<?php 
namespace Google\Service\Manager;
class FirewallModule extends \Google\Collection
{
  protected $allowedType = 'Google\Service\Manager\AllowedRule';
  protected $allowedDataType = 'array';
  public $description;
  public $network;
  public $sourceRanges;
  public $sourceTags;
  public $targetTags;

  public function setAllowed($allowed)
  {
    $this->allowed = $allowed;
  }

  public function getAllowed()
  {
    return $this->allowed;
  }

  public function setDescription($description)
  {
    $this->description = $description;
  }

  public function getDescription()
  {
    return $this->description;
  }

  public function setNetwork($network)
  {
    $this->network = $network;
  }

  public function getNetwork()
  {
    return $this->network;
  }

  public function setSourceRanges($sourceRanges)
  {
    $this->sourceRanges = $sourceRanges;
  }

  public function getSourceRanges()
  {
    return $this->sourceRanges;
  }

  public function setSourceTags($sourceTags)
  {
    $this->sourceTags = $sourceTags;
  }

  public function getSourceTags()
  {
    return $this->sourceTags;
  }

  public function setTargetTags($targetTags)
  {
    $this->targetTags = $targetTags;
  }

  public function getTargetTags()
  {
    return $this->targetTags;
  }
}
