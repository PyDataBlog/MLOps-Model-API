#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>
#include <iostream>

int main()
{
    sf::RenderWindow window(sf::VideoMode(200, 200), "Donguili");

    sf::Music sound;
    if (!sound.openFromFile("C:/Users/Alpha/Documents/Donguili/bin/Debug/test.ogg"))
    return -1; // erreur
    sound.play();

    while (window.isOpen())
    {
        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
                window.close();
        }
// Presse de touche

     if (sf::Keyboard::isKeyPressed(sf::Keyboard::Space))
    {
        std::cout << "Musique en pause" << std::endl;
        sound.pause();
    }
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::P))
    {
        std::cout << "Musique lancé" << std::endl;
         sound.play();
     }

// Fin Presse touche
        window.clear();
        window.display();
    }

    return 0;
}
