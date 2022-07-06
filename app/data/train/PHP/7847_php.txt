<?php

namespace App\Http\Controllers;

use Illuminate\Routing\Controller as BaseController;
use App\Countries;
use Request;
use App\Http\Requests\CountriesRequest;
use Validator;

class CountriesController extends BaseController
{
    /**
    * function - get main page
    *
    * @return all what database has
    */
 
    public function index()
    {
        $countries = Countries::all();
        return view('countries/countriesIndex', ['countries' => $countries]);
    }
     /**
    * function - create new country
    *
    * @return link on page
    */

    public function create()
    {
        return view('countries/countriesCreate');
    
    }
    /**
    * function - store new country
    *
    * @param CountriesRequest $new current
    *
    * @return link on view
    */

    public function store(CountriesRequest $new)
    {
         $country = new Countries();
         $country->identity = $new['identity'];
         $country->name = $new['name'];
         $country->nameanime = $new['nameanime'];
         $country->language = $new['language'];
         $country->area = $new['area'];
         $country->year = $new['year'];
         $country->sibling = $new['sibling'];
         $country->save();
        \Session::flash('flash_message', 'Create successful!');
        return redirect('countries');
    }
    /**
    * function - show country
    *
    * @param int $id current
    *
    * @return link on view
    */
    public function show($id)
    {
        $country = Countries::find($id);
        return view('countries/countriesShow')->with('country', $country);
    }
    /**
    * function - edit country
    *
    * @param int $id current
    *
    * @return link on view
    */
    public function edit($id)
    {
        $country = Countries::find($id);
        return view('countries/countriesEdit')->with('country', $country);
    }
    /**
    * function - update country
    *
    * @param int $id current
    *
    * @return link on view
    */
    public function update($id)
    {
        $new = Request::only(
            'identity',
            'name',
            'nameanime',
            'language',
            'area',
            'year',
            'sibling'
        );
        $country = Countries::find($id);
         
        $validator = Validator::make(
            $new,
            [
                 'identity'  => 'Required|Unique:countries',
                 'name'      => 'Required|Min:3|Max:80',
                 'nameanime' => 'Required|Min:3|Max:80',
                 'language'  => 'Required|Min:3|Max:80',
                 'area'      => 'Integer|Min:1',
                 'year'      => 'Integer|Min:1',
                 'sibling'   => 'Required|Min:3|Max:80'
            ]
        );
        if ($validator->fails()) {
            \Session::flash('flash_message', 'Something wrong!');
            return redirect()->back();
        }

         $country->identity = $new['identity'];
         $country->name = $new['name'];
         $country->nameanime = $new['nameanime'];
         $country->language = $new['language'];
         $country->area = $new['area'];
         $country->year = $new['year'];
         $country->sibling = $new['sibling'];
         $country->save();
        \Session::flash('flash_message', 'Update successful!');
         return redirect('countries');
    }

    /**
    * function - delete country
    *
    * @param int $id current
    *
    * @return link on view
    */
    public function destroy($id)
    {
        $country=Countries::find($id);
        $country->delete();
        return redirect('countries');
    }
}
