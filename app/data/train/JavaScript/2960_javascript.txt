
module.exports = function Boot(game) {
  return {
    preload: function(){
      game.load.image('mars', '/assets/images/mars.png');
    },
    create: function(){
      //This is just like any other Phaser create function
      console.log('Boot was just loaded');


      this.mars = game.add.sprite(0, 0, 'mars');
    },
    update: function(){
      //Game logic goes here
      this.mars.x += 1;
      this.mars.y += 1;
    }
  }
};
