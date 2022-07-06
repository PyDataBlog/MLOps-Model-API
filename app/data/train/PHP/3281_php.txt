@extends('defaults.masterpage')

@section('header')

{{ HTML::style('css/main.css') }}

 <title>Edit Booking</title>

@stop



@section('content')

<center>
<div id="cover"  >
{{Form::open(['route'=>'booking.store'])}}

<table id = "tabler"  >
	    <tr>

      <th colspan = "2"> Fill The Form To Edit An Appoindment</th>

        </tr>
        <tr>

             <td> {{Form::label('file_number','File Number:')}}</td>

             <td> {{Form::input('text','file_number')}}{{ $errors->first('name', '<span class=error>:message></span>') }}</td>
        
        </tr>
        <tr>

             <td>{{Form::label('section_to_visit','Visiting Section:')}}</td>

             <td>{{Form::input('text','section_to_visit')}}</td>
       
        </tr>
        <tr>

             <td>{{Form::label('doctor_assigned','Doctor Assigned:')}}</td>

             <td>{{Form::input('text','doctor_assigned')}}</td>
        </tr>
        <tr>

           <td>{{Form::submit('Book Up')}}</td>
         
        </tr>

 </table>

{{Form::close()}}

</div>

</center>

@stop
@section('footer')

<center><p>A luke Dennis production . dennisluke44@gmail.com</p></center> 

@stop