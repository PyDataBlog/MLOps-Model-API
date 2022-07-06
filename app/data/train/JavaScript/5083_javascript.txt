/**
 * Инициализация модуля. 
 */
/*global define*/
define(['./poke-control', './poke-history', 'jquery'], function (pokeControl, pokeHistory, $) {

    'use strict';

    var pokeSettings = {
            pokeControl: {
                selector: '.poke-control-container',
                template: '<form class="form"><div class="form-group"><label for="bdoChannel">Канал:</label><select class="form-control" name="bdoChannel" id="bdoChannel"></select></div><div class="form-group"><label for="bdoQuest">Квест:</label><select class="form-control" name="bdoQuest" id="bdoQuest"></select></div><button class="btn btn-success">Отправить</button></form>'
            },
            pokeHistory: {
                selector: '.poke-history-container',
                template: '<table id="eventsHistory" class="table table-condensed table-hover table-striped"><thead><tr><th data-column-id="startDate">Начало</th><th data-column-id="channel" data-type="numeric">Канал</th><th data-column-id="event">Событие</th></tr></thead></table>',
                tableSelector: '#eventsHistory'
            }
        },
        create;
    create = function (options) {
        pokeSettings = $.extend(pokeSettings, options);
        pokeControl.initialize(pokeSettings.pokeControl);
        pokeHistory.initialize(pokeSettings.pokeHistory);
    };

    return {
        create: create
    };
});