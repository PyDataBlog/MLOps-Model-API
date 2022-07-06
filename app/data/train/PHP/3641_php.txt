<?php 
namespace Google\Service\Calendar;
class Colors extends \Google\Model
{
  protected $calendarType = 'Google\Service\Calendar\ColorDefinition';
  protected $calendarDataType = 'map';
  protected $eventType = 'Google\Service\Calendar\ColorDefinition';
  protected $eventDataType = 'map';
  public $kind;
  public $updated;

  public function setCalendar($calendar)
  {
    $this->calendar = $calendar;
  }

  public function getCalendar()
  {
    return $this->calendar;
  }

  public function setEvent($event)
  {
    $this->event = $event;
  }

  public function getEvent()
  {
    return $this->event;
  }

  public function setKind($kind)
  {
    $this->kind = $kind;
  }

  public function getKind()
  {
    return $this->kind;
  }

  public function setUpdated($updated)
  {
    $this->updated = $updated;
  }

  public function getUpdated()
  {
    return $this->updated;
  }
}
