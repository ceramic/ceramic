var app = require('app');
var ipc = require('ipc');
var BrowserWindow = require('browser-window');

require('crash-reporter').start();

/* Windows */

var window_db = {};

/* Lifecycle management */

function quit() {
  app.quit();
};

app.on('window-all-closed', function() {
  if (process.platform != 'darwin') {
    // FIXME: signal that everything's closed
  }
});

/* Start up */

app.on('ready', function() {
  /* Start listening for commands on the server */

});
