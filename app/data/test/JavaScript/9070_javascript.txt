function drawCanvas() {
    var canvas = document.getElementById('canvas-container');
    var context = canvas.getContext("2d");
    var grd = context.createLinearGradient(0,0,170,0);
    grd.addColorStop(0,"red");
    grd.addColorStop(0.5,"blue");
    grd.addColorStop(1,"green");
    context.fillStyle = grd;
    context.lineWidth = 50;
    context.lineJoin="round";
    context.beginPath();
    context.arc(100,100,50,0,1.5*Math.PI);
    context.closePath();
    context.stroke();
    context.fill();
}

$(function() {
    drawCanvas();
});