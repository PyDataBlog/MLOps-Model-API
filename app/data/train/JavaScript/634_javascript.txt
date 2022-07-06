(function(app, undefined) {
  'use strict';

  if(!app) throw new Error('Application "app" namespace not found.');

  //----------------------------------------------------------------------------

  console.log( 'hello world' );
  console.log( 'Application Running...' );

  //----------------------------------------------------------------------------
  // @begin: renders

  app.render.jquery();
  app.render.vanilla();

  // @end: renders
  //----------------------------------------------------------------------------
  // @begin: to_jquery

  app.to_jquery.run();

  // @end: to_jquery
  //----------------------------------------------------------------------------
  // @begin: mustache

  app.menu.render();
  app.menu.option.reset();

  // @begin: mustache
  //----------------------------------------------------------------------------

})(window.app);
