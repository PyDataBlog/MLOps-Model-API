(function (parent, $, element) {
    function loadEvents() {
        var date = new Date();
        var d = date.getDate();
        var m = date.getMonth();
        var y = date.getFullYear();
        
        return [
            {
                title: 'Appointment 1',
                start: new Date(y, m, d, 10),
                end: new Date(y, m, d, 11),
                allDay: false,
                editable: true,
                startEditable: true,
                durationEditable: true,
                plantpot: 'coffeetable'
            },
            {
                title: 'Appointment 2',
                start: new Date(y, m, d, 11, 30),
                end: new Date(y, m, d, 12),
                allDay: false
            }
        ];
    }

    function onEventClick(event) {
        alert(event.plantpot);
        alert($(element).fullCalendar('clientEvents').length);
    }

    function render() {
        $(document).ready(function() {
            $(element).fullCalendar({
                header: {
                    left: 'prev,next today',
                    center: 'title',
                    right: 'month,agendaWeek,agendaDay'
                },
                events: loadEvents(),
                eventClick: onEventClick
            });
        });
    }

    parent.schedule = {
        load: render
    };
})(window, $, '#calendar');

window.schedule.load();