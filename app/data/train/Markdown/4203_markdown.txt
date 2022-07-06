# Evénements
L'API dispose de l'excellent paquet EventEmitter, vous pouvez donc écouter et émettre des événements très facilement. Il est fortement recommandé d'utiliser les alias fournis par l'API pour éviter d'éventuels problèmes 
de compatibilités en cas de changements.

`AC_on` et `AC_emit` utilisent un préfixe pour le nom de l'évènement, il est géré automatiquement, si vous voulez passer par les méthodes natives vous devez ajouter `AC_` devant le nom de l'événement à écouter/émettre.

## Ecouter un événement
```Javascript
AC_on('nom', callback(data));
```

## Emettre un événement
```Javascript
AC_emit('nom', data);
```

## Exemple
```Javascript
// On écoute l'événement `pizza`
AC_on('Gecko', function(message) {
  console.log(message);
});

// On émet l'événement `Gecko`
AC_emit('Gecko', 'ce mec est top!');
```

Cet exemple affichera `ce mec est top!` dans la console du navigateur

###### Navigation rapide
* [Retour au sommaire des AnoCheat](./readme.md)
* [Retour à l'index]('../readme.md)