#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include "mi_allegro.h"
#include "mario.h"

#define N 120

#define SPRITESHEET "sprites/loco.png"
#define BACKGROUND "sprites/fondo.png"
#define DELTA 0.25

enum
{ KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT };

int
main (int argc, char **argv)
{
    Mario mario;
    ALLEGRO_BITMAP *bm = NULL;
    ALLEGRO_BITMAP *bm2 = NULL;
    bool tecla[4] = { false, false, false, false };
    int sprite_number = 0;
    srand (time (NULL));
    iniciar_allegro ();
    
    bm = al_load_bitmap (SPRITESHEET);
    bm2 = al_load_bitmap (BACKGROUND);
    
    if (!bm or !bm2)
    {
	al_show_native_message_box (display, "Error", "Error",
		"No se ha podido crear el bitmap", NULL,
		ALLEGRO_MESSAGEBOX_ERROR);
	al_destroy_timer (timer);
	al_destroy_display (display);
	exit (EXIT_FAILURE);
    }
    
    mario.set_dibujo (bm);
    
    while (1)
    { /* Buzz Lightyear */
	ALLEGRO_EVENT ev;
	ALLEGRO_TIMEOUT timeout;
	al_init_timeout (&timeout, 0.06);
	
	bool get_event = al_wait_for_event_until (event_queue, &ev, &timeout);
	
	if (get_event)
	{
	    if (ev.type == ALLEGRO_EVENT_DISPLAY_CLOSE)
		break;
	    if (ev.type == ALLEGRO_EVENT_TIMER)
		redraw = true;
	    if (ev.type == ALLEGRO_EVENT_KEY_DOWN)
	    {
		switch (ev.keyboard.keycode)
		{
		    case ALLEGRO_KEY_UP:
			tecla[KEY_UP] = true;
			break;
		    case ALLEGRO_KEY_DOWN:
			tecla[KEY_DOWN] = true;
			break;
		    case ALLEGRO_KEY_LEFT:
			tecla[KEY_LEFT] = true;
			break;
		    case ALLEGRO_KEY_RIGHT:
			tecla[KEY_RIGHT] = true;
			break;
		}
	    }
	    if (ev.type == ALLEGRO_EVENT_KEY_UP)
	    {
		switch (ev.keyboard.keycode)
		{
		    case ALLEGRO_KEY_UP:
			tecla[KEY_UP] = false;
			break;
		    case ALLEGRO_KEY_DOWN:
			tecla[KEY_DOWN] = false;
			break;
		    case ALLEGRO_KEY_LEFT:
			tecla[KEY_LEFT] = false;
			break;
		    case ALLEGRO_KEY_RIGHT:
			tecla[KEY_RIGHT] = false;
			break;
		    case ALLEGRO_KEY_LSHIFT:
			sprite_number++;
			break;
		}
	    }
	}
	/* Actualizar las coordenadas de la pelota */
	if (tecla[KEY_UP])
	    mario.change_vy(-DELTA);
	if (tecla[KEY_DOWN])
	    mario.change_vy(DELTA);
	if (tecla[KEY_LEFT])
	    mario.change_vx(-DELTA);
	if (tecla[KEY_RIGHT])
	    mario.change_vx(DELTA);
	
	mario.actualizate();
	
	if (mario.gameover() == 0)
	{
	    return 0;
	}

	if (redraw && al_is_event_queue_empty (event_queue))
	{
	    al_clear_to_color (al_map_rgb (0, 0, 0));
	    al_draw_bitmap (bm2, 0, 0, 0);
	    for (int i = 0; i < N; i++)
		al_draw_bitmap (mario.get_dibujo (),  mario.get_x (), 
			mario.get_y (), 0);
	    al_flip_display ();
	    redraw = false;
	}
    }

    al_destroy_bitmap (bm);
    al_destroy_bitmap (bm2);
    destruir_allegro ();
    
    return 0;
}
