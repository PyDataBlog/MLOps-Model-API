# Задание

**Используйте Function#bind** для реализации логирующей функции.

Вам необходимо создать функцию, которая будет логировать некоторую информацию с заданным неймспейсом.

## Аргументы

* namespace: строка, которая вставляется в начало сообщения.

## Пример

```js
var info = logger('INFO:')
info('this is an info message')
// INFO: this is an info message

var warn = logger('WARN:')
warn('this is a warning message', 'with more info')
// WARN: this is a warning message with more info

```

## Условия

* Использовать Function#bind

## Шаблон

```js

function logger(namespace) {
  // SOLUTION GOES HERE
}

var zeroFill = function (num) { return num < 10 ? '0' + num : num },
	currentTime = new Date(),
	logTime = '[' 
		+ zeroFill(currentTime.getHours()) 
		+ ':' + zeroFill(currentTime.getMinutes()) 
		+ ':' + zeroFill(currentTime.getSeconds())
		+ ']',
	log = logger(logTime);
log('Роман пришел на занятие'); // [18:00:01] Роман пришел на занятие
```

## Ресурсы

* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/bind
