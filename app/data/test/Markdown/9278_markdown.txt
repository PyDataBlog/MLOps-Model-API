LEEME

    Para aprender sobre el rst ( reStructuredText ), mirar aquí, por ejemplo:

        http://es.wikieducator.org/Usuario:Lmorillas/modulo_lenguajes_de_marcas/ligeros/rst


Instalar este proyecto

1) El entorno

    * Esto está hecho en Python

    * No es necesario, pero si recomendable, tener conocimientos de
      Python

    * Se necesita tener instalado un entorno virtual para Python ( ver
      → http://rukbottoland.com/tutoriales/tutorial-de-python-virtualenvwrapper/
      ). Por convenio, solemos usar ~/Proyectos/Python/
      como directorio base para este tipo de proyectos.

2) Clonar el proyecto

    * mkdir -p ~/Proyectos/Python/

    * cd ~/Proyectos/Python/

    * git clone https://github.com/equipote/j19j.git

3) Creación del entorno virtual para este proyecto

    * mkvirtualenv -a ~/Proyectos/Python/j19j --no-site-packages j19j

    * Tras crear el entorno, automagicamente nos quedaremos dentro de
      él.

4) Instalación de requisitos

    * pip install -r requerimientos.txt

5) Actualizar submodulos:

    * git submodule add https://github.com/getpelican/pelican-themes temas

    * git submodule add https://github.com/getpelican/pelican-plugins plugins

    * git submodule update --init --recursive


6) Creación del directorio de salida (si no existe)

    * mkdir output

7) Generación de las páginas estáticas

    * make html

8) Ver las páginas estáticas

    make serve

