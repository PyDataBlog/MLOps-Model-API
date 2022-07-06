<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Http\Requests\TicketFormRequest;
use App\Ticket;
use Illuminate\Support\Facades\Mail;

class TicketsController extends Controller
{
    /**
     * Display a listing of the resource.
     *
     * @return \Illuminate\Http\Response
     */
    public function index()
    {
    	$tickets = Ticket::all();
	//compact devuelve un array de tickets
    	return view('tickets.index', compact('tickets'));
    }

    /**
     * Show the form for creating a new resource.
     *
     * @return \Illuminate\Http\Response
     */
    public function create()
    {
         return view('tickets.create');
    }

    /**
     * Store a newly created resource in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @return \Illuminate\Http\Response
     */
    //TicketFormRequest obligamos a que valide los campos del formulario
    public function store(TicketFormRequest  $request)
    {
       $slug = uniqid();
       $ticket = new Ticket(array(
        'title' => $request->get('title'),
        'content' => $request->get('content'),
        'slug' => $slug
        ));
       $ticket->save();
       $data = array(
        'ticket' => $slug,
         );

         Mail::send('emails.ticket', $data, function ($message) {
            $message->from('javimartinalonso@gmail.com', 'Curso Laravel');

            $message->to('javimartinalonso@gmail.com')->subject('¡Hay un nuevo ticket!');
         });
       return redirect('/contact')-> with('status', 'Su Ticket ha sido creado. Su id única es: ' .$slug);

    }

    /**
     * Display the specified resource.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */
    public function show($slug)
    {
    $ticket = Ticket::whereSlug($slug)->firstOrFail();
    $comments = $ticket->comments()->get();
    return view('tickets.show', compact('ticket', 'comments'));
    }

    /**
     * Show the form for editing the specified resource.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */
    public function edit($slug)
    {
    	$ticket = Ticket::whereSlug($slug)->firstOrFail();
    	return view('tickets.edit', compact('ticket'));
    }

    /**
     * Update the specified resource in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */
    public function update($slug, TicketFormRequest $request)
    {
        $ticket = Ticket::whereSlug($slug)->firstOrFail();
        $ticket->title = $request->get('title');
        $ticket->content = $request->get('content');
        
        if($request->get('status') != null) {
            $ticket->status = 0;
        } else {
            $ticket->status = 1;
        }
    
        $ticket->save();
        
        return redirect(action('TicketsController@edit', $ticket->slug))->with('status', '¡El ticket '.$slug.' ha sido actualizado!');
    }

    /**
     * Remove the specified resource from storage.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */
    public function destroy($slug)
    {
       $ticket = Ticket::whereSlug($slug)->firstOrFail();
       $ticket->delete();
       
       return redirect('/tickets')->with('status', 'El ticket '.$slug.' ha sido borrado');
   }
}
