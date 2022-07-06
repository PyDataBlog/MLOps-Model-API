var width = window.innerWidth,
    height = window.innerHeight,
    boids = [],
    destination,
    canvas,
    context;
const MAX_NUMBER = 100;
const MAX_SPEED = 1;
const radius = 5;

init();
animation();

function init(){
  canvas = document.getElementById('canvas'),
  context = canvas.getContext( "2d" );
  canvas.width = width;
  canvas.height = height;
  destination = {
    x:Math.random()*width,
    y:Math.random()*height
  };
  for (var i = 0; i <MAX_NUMBER; i++) {
    boids[i] = new Boid();
  };
}

var _animation;
function animation(){
  _animation = requestAnimationFrame(animation);
  context.clearRect(0,0,width,height);

  for (var i = 0; i < boids.length; i++) {
    boids[i].rule1();
    boids[i].rule2();
    boids[i].rule3();
    boids[i].rule4();
    boids[i].rule5();
    boids[i].rule6();

    var nowSpeed = Math.sqrt(boids[i].vx * boids[i].vx + boids[i].vy * boids[i].vy );
    if(nowSpeed > MAX_SPEED){
      boids[i].vx *= MAX_SPEED / nowSpeed;
      boids[i].vy *= MAX_SPEED / nowSpeed; 
    }
    boids[i].x += boids[i].vx;
    boids[i].y += boids[i].vy;

    drawCircle(boids[i].x,boids[i].y);
    drawVector(boids[i].x,boids[i].y,boids[i].vx,boids[i].vy);
  };
}
/*
//mouseEvent
document.onmousemove = function (event){
  destination ={
    x:event.screenX,
    y:event.screenY
  }
};
*/

function Boid(){
  this.x = Math.random()*width;
  this.y = Math.random()*height;
  this.vx = 0.0;
  this.vy = 0.0;
  this.dx = Math.random()*width;
  this.dy = Math.random()*height;
  //群れの中心に向かう
  this.rule1 = function(){
    var centerx = 0,
        centery = 0;
    for (var i = 0; i < boids.length; i++) {
      if (boids[i] != this) {
        centerx += boids[i].x;
        centery += boids[i].y;
      };
    };
    centerx /= MAX_NUMBER-1;
    centery /= MAX_NUMBER-1;
    this.vx += (centerx-this.x)/1000;
    this.vy += (centery-this.y)/1000;
  }

  //他の個体と離れるように動く
  this.rule2 = function(){
    var _vx = 0,
        _vy = 0;
    for (var i = 0; i < boids.length; i++) {
      if(this != boids[i]){
        var distance = distanceTo(this.x,boids[i].x,this.y,boids[i].y);

        if(distance<25){
          distance += 0.001;
          _vx -= (boids[i].x - this.x)/distance;
          _vy -= (boids[i].y - this.y)/distance; 
          //this.dx = -boids[i].x;
          //this.dy = -boids[i].y;
        }
      }
    };
    this.vx += _vx;
    this.vy += _vy; 
  }

  //他の個体と同じ速度で動こうとする
  this.rule3 = function(){
    var _pvx = 0,
        _pvy = 0;
    for (var i = 0; i < boids.length; i++) {
      if (boids[i] != this) {
        _pvx += boids[i].vx;
        _pvy += boids[i].vy;
      }
      };
      _pvx /= MAX_NUMBER-1;
      _pvy /= MAX_NUMBER-1;

      this.vx += (_pvx - this.vx)/10;
      this.vy += (_pvy - this.vy)/10;
    };

  //壁側の時の振る舞い
  this.rule4 = function(){
    if(this.x < 10 && this.vx < 0)this.vx += 10/(Math.abs( this.x  ) + 1 );
    if(this.x > width && this.vx > 0)this.vx -= 10/(Math.abs( width - this.x ) + 1 );
    if (this.y < 10 && this.vy < 0)this.vy += 10/(Math.abs( this.y  ) + 1 );
    if(this.y > height && this.vy > 0)this.vy -= 10/(Math.abs( height - this.y ) + 1 );
    };

  //目的地に行く
  this.rule5 = function(){
    var _dx = this.dx - this.x,
        _dy = this.dy - this.y;
    this.vx += (this.dx - this.x)/500;
    this.vy += (this.dy - this.y)/500;
  }

  //捕食する
  this.rule6 = function(){
    var _vx = Math.random()-0.5,
        _vy = Math.random()-0.5;
    for (var i = 0; i < boids.length; i++) {
      if(this != boids[i] && this.dx != boids[i].dx && this.dy != boids[i].dy){
        var distance = distanceTo(this.x,boids[i].x,this.y,boids[i].y);

        if(distance<20 && distance>15){
          console.log(distance);
          distance += 0.001;
          _vx += (boids[i].x - this.x)/distance;
          _vy += (boids[i].y - this.y)/distance;
          drawLine(this.x,this.y,boids[i].x,boids[i].y);
          this.dx = boids[i].dx;
          this.dy = boids[i].dy;
        }
      }
    };
    this.vx += _vx;
    this.vy += _vy; 
  }
}

function distanceTo(x1,x2,y1,y2){
  var dx = x2-x1,
      dy = y2-y1;
  return Math.sqrt(dx*dx+dy*dy);
}

function drawCircle(x,y){
  context.beginPath();
  context.strokeStyle = "#fff";
  context.arc(x,y,radius,0,Math.PI*2,false);
  context.stroke();
}

const VectorLong = 10;
function drawVector(x,y,vx,vy){
  context.beginPath();
  var pointx = x+vx*VectorLong;
  var pointy = y+vy*VectorLong;
  context.moveTo(x,y);
  context.lineTo(pointx,pointy);
  context.stroke();
}

function drawLine(x1,y1,x2,y2){
  context.beginPath();
  context.moveTo(x1,y1);
  context.lineTo(x2,y2);
  context.stroke();
}

