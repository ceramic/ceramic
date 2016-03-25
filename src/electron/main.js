var app = require('app');
var ipc = require('ipc');
var BrowserWindow = require('browser-window');

require('crash-reporter').start();

/* Communication */

var RemoteJS = {};

function startWebSockets(port) {
  RemoteJS.ws = new WebSocket('ws://localhost:' + port);

  RemoteJS.send = function(data) {
    RemoteJS.ws.send(data);
  };

  RemoteJS.ws.onmessage = function(evt) {
    eval(evt.data);
  };
  RemoteJS.ws.onopen = function() {
    RemoteJS.send('connected');
  };
};

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
  // Start the WebSockets server
  startWebSockets(progress.argv[2]);
});
