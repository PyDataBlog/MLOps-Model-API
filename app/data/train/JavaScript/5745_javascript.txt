import robot from 'robotjs'
import sleep from 'sleep'
import orifice from './fc8_orifice_map.json'
var fs = require('fs')

/*//set speed
robot.setKeyboardDelay(150)
robot.setMouseDelay(100)*/

let type = [
    "Orifice",
    "Venturi",
    "Nozzle",
    "Fixed Geometry",
    "V-Cone",
    "Segmental Meters",
    "Linear Meters"
]

exports.startFC8 = function(){
    robot.keyTap('command')
    robot.typeString('FC8')
    sleep.msleep(500)
    robot.keyTap('enter')
    sleep.msleep(500)
}

exports.selectType = function(request){
    robot.keyTap('tab')
    let i = 0
    for(i=0; i<type.indexOf(request); i++){
        robot.keyTap('down')
    }
    robot.keyTap('tab')
    robot.keyTap('enter')
}

exports.autoCompileGas = function(){
    let i = 0
    for(i=0; i<9; i++){
        robot.keyTap('tab')
    }
    robot.keyTap('enter')
    for(i=0; i<7; i++){
        robot.keyTap('tab')
    }
    robot.keyTap('enter')
    robot.keyTap('enter')
    robot.keyTap('tab')
    robot.keyTap('tab')
    robot.keyTap('enter')
    robot.keyTap('enter')
}

exports.calculation = function(object) {
    //select dp flow size
    let i = 0
    if(object.method == "dp"){
        robot.keyTap('enter')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.dp.percMaxFlow)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.dp.maxFlow)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.dp.normalFlow)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.pipeDiameter)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.borePrimaryElement)
        robot.keyTap('tab')
        robot.keyTap('enter')
    } else if (object.method == "flow"){
        robot.keyTap('tab')
        robot.keyTap('enter')
        robot.keyTap('tab')
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.flow.differentialPressure)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.pipeDiameter)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.borePrimaryElement)
        robot.keyTap('tab')
        robot.keyTap('tab')
        robot.keyTap('enter')
    } else if (object.method == "size"){
        robot.keyTap('tab')
        robot.keyTap('tab')
        robot.keyTap('enter')
        robot.keyTap('tab')
        robot.keyTap('tab')
        /*for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.size.percMaxFlow)*/
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.size.maxFlow)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.size.normalFlow)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.size.differential)
        robot.keyTap('tab')
        for(i=0; i<10; i++){
            robot.keyTap('delete')
        }
        robot.typeString(object.base.pipeDiameter)
        robot.keyTap('tab')
        robot.keyTap('tab')
        
        robot.keyTap('enter')
    }
    if(object.checkVentDrainHole!="off"){
        
        /*
        robot.keyTap('tab')
        robot.keyTap('tab')
        let ind = orifice.FlangeTaps.VentDrainHole.indexOf(object.ventDrainHole)
        let indStd = orifice.FlangeTaps.VentDrainHole.indexOf('No Vent/Drain Hole')
        if(ind<indStd){
            for (i=0; i<indStd-ind; i++){
                robot.keyTap('up')
            }
        } else {
            for (i=0; i<ind-indStd; i++){
                robot.keyTap('down')
            }
        }*/
        
        if(object.ventDrainHole=="FC8"){
            robot.keyTap('enter')
        } else if (object.ventDrainHole=="userdefined"){
            robot.keyTap('tab')
            robot.keyTap('tab')
            for(i=0;i<17;i++){
                robot.keyTap('down')
            }
            robot.typeString(object.userDefinedDiameters)
            robot.keyTap('tab')
            robot.keyTap('enter')
            robot.keyTap('tab')
            robot.keyTap('enter')
        }
        for(i=0; i<2; i++){
            robot.keyTap('tab', 'shift')
        }
    }
}

exports.printPDF = function(fileName, customer, tag){
    let i=0
    robot.keyTap('enter')
    for(i=0; i<7; i++){
        robot.keyTap('tab')
    }
    for(i=0; i<10; i++){
        robot.keyTap('delete')
    }
    robot.typeString(customer)
    robot.keyTap('tab')
    robot.keyTap('tab')
    robot.typeString(tag)
    for(i=0; i<15; i++){
        robot.keyTap('tab')
    }
    robot.keyTap('enter')
    sleep.msleep(500)
    robot.typeString(fileName)
    robot.keyTap('enter')
    sleep.msleep(500)
}

exports.existFile = function (fileName, errors) {
    fs.exists('../Documents/'+fileName, function(exists) {
        if(!exists){
            errors.push(fileName)
            console.log(fileName, "ha presentato un errore")
            console.log(errors)
        } 
    });
}
