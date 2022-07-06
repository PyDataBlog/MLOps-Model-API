<?php

class AuthController extends BaseController {

  public function status() {
    return Response::json(MitcoeUser::check());
  }

  public function login()
  {
    $userdata = array(
        'email'   => Input::get('email'),
        'password'  => Input::get('password')
      );
  
      if (Auth::attempt($userdata)) {

        // validation successful!
        // redirect them to the secure section or whatever
        // return Redirect::to('secure');
        // for now we'll just echo success (even though echoing in a controller is bad)
       return Response::json(array('flash'=>'successfully logged in'));

      } else {    

        // validation not successful, send back to form 
         return Response::json(array('flash' => 'Invalid username or password'), 400);

      }
   //  if(MitcoeUser::getlogin(array('email' => Input::json('email'), 'password' => Input::json('password'))))
   //  {
   //    return Response::json(array('flash'=>'successfully logged in'));
   // } else {
   //    return Response::json(array('flash' => 'Invalid username or password'), 400);
   //  }
  }

  public function logout()
  {
    MitcoeUser::logout();
    return Response::json(array('flash' => 'Logged Out!'));
  }

}

