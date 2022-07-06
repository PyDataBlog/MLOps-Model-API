<?php

namespace Bk\MeteoBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;

class MeteoController extends Controller {

    public function indexAction() {

        $param = $this->container->getParameter('OpenWeatherMap');
        /*
        $fake = '../src/Bk/MeteoBundle/cache/fake/france.json';
        $json = json_decode(file_get_contents($fake));
        $jsonCities = $json->list;
        $cities = array();
        foreach ($jsonCities as $jsonCity) {
            var_dump($jsonCity);
            $weather = new \Bk\MeteoBundle\Entity\Weather();
            $weather
                    ->setDate($jsonCity->dt)
                    ->setSunrise($jsonCity->sys->sunrise)
                    ->setSunset($jsonCity->sys->sunset)
                    ->setTemperature($jsonCity->main->temp)
                    ->setTemperatureMin($jsonCity->main->temp_min)
                    ->setTemperatureMax($jsonCity->main->temp_max)
                    ->setPressure($jsonCity->main->pressure)
                    ->setHumidity($jsonCity->main->humidity)
                    ->setWindSpeed($jsonCity->wind->speed)
                    ->setWindDirectionValue($jsonCity->wind->deg)
                    ->setCloudsCover($jsonCity->clouds->all)
                    ->setCloudsDescription($jsonCity->weather[0]->description)
            ;
            $city = new \Bk\MeteoBundle\Entity\City();
            $city
                    ->setName($jsonCity->name)
                    ->setLongitude($jsonCity->coord->lon)
                    ->setLatitude($jsonCity->coord->lat)
                    ->addWeather($weather)
            ;
            var_dump($city->getWeathers());
            var_dump($city);
        }
         *
         */
        return $this->render('BkMeteoBundle:Meteo:index.html.twig', array('cities' => $param['city']));
    }

}
