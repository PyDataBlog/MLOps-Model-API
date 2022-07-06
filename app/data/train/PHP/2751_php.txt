<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="width=device-width, initial-scale=1" name="viewport">
    <title>Lavue</title>
    <!-- fonts -->
    <link href="https://fonts.googleapis.com/css?family=Raleway:100,600"
      rel="stylesheet" type="text/css">
    <link href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.5.0/css/font-awesome.min.css"
      rel="stylesheet">

    <link href="{{asset('css/bulma.css')}}" rel="stylesheet"/>
  <body>
      @yield('content')

      <script src="{{asset('js/utils.js')}}"></script>
  </body>
</html>
