<?php

  //by default this model will link to the cards table
  class Side extends Eloquent
  {
     protected $fillable = array('title','text');
    
    public function card()
    {
        return $this->belongsTo('Card');
    }
    /* 
    public function phone()
    {
      return $this->hasOne('Card');
    }
    */
  }
