var app = require('app');  // Module to control application life.
var BrowserWindow = require('browser-window');  // Module to create native browser window.

// Report crashes to our server.
// require('crash-reporter').start();

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is GCed.
var mainWindow = null;

// Quit when all windows are closed.
app.on('window-all-closed', function() {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform != 'darwin') {
    app.quit();
  }
});

// Specify flash path.
// On Windows, it might be /path/to/pepflashplayer.dll
// On Mac, /path/to/PepperFlashPlayer.plugin
// On Linux, /path/to/libpepflashplayer.so
// app.commandLine.appendSwitch('ppapi-flash-path', '/path/to/libpepflashplayer.so');
app.commandLine.appendSwitch('ppapi-flash-path', '/Applications/Google Chrome.app/Contents/Versions/44.0.2403.125/Google Chrome Framework.framework/Internet Plug-Ins/PepperFlash/PepperFlashPlayer.plugin');

// Specify flash version, for example, v17.0.0.169
app.commandLine.appendSwitch('ppapi-flash-version', '18.0.0.209');

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
app.on('ready', function() {
  // Create the browser window.
  mainWindow = new BrowserWindow({
    'accept-first-mouse': true,
    'always-on-top': true,
    'auto-hide-menu-bar': true,
    'dark-theme': true,
    'frame': false,
    'height': 600,
    'resizable': true,
    'show': false,
    'title': 'StreamShell',
    'width': 800,
    'web-preferences': {
      'plugins': true
    }
  });

  // and load the index.html of the app.
  mainWindow.loadUrl('file://' + __dirname + '/index.html');

  // Emitted when the window is closed.
  mainWindow.on('closed', function() {
    // Dereference the window object
    mainWindow = null;
  });

  // Display the window only once the DOM is ready
  mainWindow.webContents.on('did-finish-load', function() {
    setTimeout(function() {
      mainWindow.show();
      // mainWindow.openDevTools({detach: true});
    }, 40);
  });

});
