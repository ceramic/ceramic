var app = require('app');
var ipc = require('ipc');
var BrowserWindow = require('browser-window');
var WebSocket = require('ws');

require('crash-reporter').start();

var Ceramic = {};

/* Communication */

var RemoteJS = {};

Ceramic.startWebSockets = function(port) {
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

Ceramic.syncEval = function(id, fn) {
  const result = fn();
  RemoteJS.send(JSON.stringify({
    id: id,
    result: result
  }))
};

/* Windows */

Ceramic.window_db = {};

/* Lifecycle management */

Ceramic.quit = function() {
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
  Ceramic.startWebSockets(process.argv[2]);
});
