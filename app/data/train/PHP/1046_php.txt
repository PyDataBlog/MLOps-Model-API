<?php
/**
 * TrcIMPLAN Índice Básico de Colonias
 *
 * Copyright (C) 2016 Guillermo Valdes Lozano
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * @package TrcIMPLAN
 */

namespace IBCTorreon;

/**
 * Clase VillaJardin
 */
class VillaJardin extends \IBCBase\PublicacionWeb {

    /**
     * Constructor
     */
    public function __construct() {
        // Título, autor y fecha
        $this->nombre          = 'Villa Jardin';
        $this->autor           = 'IMPLAN Torreón Staff';
        $this->fecha           = '2016-09-02 12:55:35';
        // El nombre del archivo a crear (obligatorio) y rutas relativas a las imágenes
        $this->archivo         = 'villa-jardin';
        $this->imagen          = '../imagenes/imagen.jpg';
        $this->imagen_previa   = '../imagenes/imagen-previa.jpg';
        // La descripción y claves dan información a los buscadores y redes sociales
        $this->descripcion     = 'Colonia Villa Jardin de IBC Torreón.';
        $this->claves          = 'IMPLAN, Torreon, Desagregación';
        // El directorio en la raíz donde se guardará el archivo HTML
        $this->directorio      = 'ibc-torreon';
        // Opción del menú Navegación a poner como activa cuando vea esta publicación
        $this->nombre_menu     = 'IBC > IBC Torreón';
        // Para el Organizador
        $this->categorias      = array();
        $this->fuentes         = array();
        $this->regiones        = array();
    } // constructor

    /**
     * Datos
     *
     * @return array Arreglo asociativo
     */
    public function datos() {
        return array(
            'Demografía' => array(
                'Población total' => '548',
                'Porcentaje de población masculina' => '46.41',
                'Porcentaje de población femenina' => '53.59',
                'Porcentaje de población de 0 a 14 años' => '11.60',
                'Porcentaje de población de 15 a 64 años' => '63.71',
                'Porcentaje de población de 65 y más años' => '13.63',
                'Porcentaje de población no especificada' => '11.06',
                'Fecundidad promedio' => '1.93',
                'Porcentaje de población nacida en otro estado' => '24.29',
                'Porcentaje de población con discapacidad' => '4.61'
            ),
            'Educación' => array(
                'Grado Promedio de Escolaridad' => '14.01',
                'Grado Promedio de Escolaridad masculina' => '14.57',
                'Grado Promedio de Escolaridad femenina' => '13.51'
            ),
            'Características Económicas' => array(
                'Población Económicamente Activa' => '52.20',
                'Población Económicamente Activa masculina' => '63.52',
                'Población Económicamente Activa femenina' => '36.48',
                'Población Ocupada' => '93.39',
                'Población Ocupada masculina' => '63.47',
                'Población Ocupada femenina' => '36.53',
                'Población Desocupada' => '6.61',
                'Derechohabiencia' => '72.90'
            ),
            'Viviendas' => array(
                'Hogares' => '154',
                'Hogares Jefatura masculina' => '80.29',
                'Hogares Jefatura femenina' => '19.71',
                'Ocupación por Vivienda' => '3.56',
                'Viviendas con Electricidad' => '99.72',
                'Viviendas con Agua' => '100.00',
                'Viviendas con Drenaje' => '99.72',
                'Viviendas con Televisión' => '89.25',
                'Viviendas con Automóvil' => '84.62',
                'Viviendas con Computadora' => '72.81',
                'Viviendas con Celular' => '82.00',
                'Viviendas con Internet' => '68.86'
            ),
            'Unidades Económicas' => array(
                'Total Actividades Económicas' => '52',
                'Primer actividad nombre' => 'Otros servicios, excepto Gobierno',
                'Primer actividad porcentaje' => '15.38',
                'Segunda actividad nombre' => 'Inmobiliarios',
                'Segunda actividad porcentaje' => '13.46',
                'Tercera actividad nombre' => 'Comercio Menudeo',
                'Tercera actividad porcentaje' => '11.54',
                'Cuarta actividad nombre' => 'Salud',
                'Cuarta actividad porcentaje' => '7.69',
                'Quinta actividad nombre' => 'Profesionales, Científicos, Técnicos',
                'Quinta actividad porcentaje' => '7.69'
            )
        );
    } // datos

} // Clase VillaJardin

?>
