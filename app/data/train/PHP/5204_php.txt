<?php namespace App\Handlers\Events;

use App\Events\ContactRequest;

use Illuminate\Queue\InteractsWithQueue;
use Illuminate\Contracts\Queue\ShouldBeQueued;

class SendContactSms {

	/**
	 * Create the event handler.
	 *
	 * @return void
	 */
	public function __construct()
	{
		//
	}

	/**
	 * Handle the event.
	 *
	 * @param  ContactRequest  $event
	 * @return void
	 */
	public function handle(ContactRequest $event)
	{
		//
	}

}
