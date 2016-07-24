const electron = require('electron');
const app = electron.app;
const BrowserWindow = electron.BrowserWindow;
const WebSocket = require('ws');
const dialog = require('electron').dialog;

var Ceramic = {
    dialog: dialog
};

/* Communication */

var RemoteJS = {};

Ceramic.startWebSockets = function(address, port) {
  RemoteJS.ws = new WebSocket('ws://' + address + ':' + port);

  RemoteJS.send = function(data) {
    RemoteJS.ws.send(data);
  };

  RemoteJS.ws.onmessage = function(evt) {
    const js = evt.data;
    try {
      eval(js);
    } catch (err) {
      dialog.showErrorBox('JavaScript Error', 'Error evaluating JavaScript from Ceramic: ' + js);
    }
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

Ceramic.startCrashReporter = function (options) {
    electron.crashReporter.start(options);
};

/* Windows */

Ceramic.windows = {};

Ceramic.createWindow = function(url, options) {
  var copy = options;
  copy.show = false;

  var win = new BrowserWindow(options);
  if (url) {
    win.loadURL(url);
  };
  return win;
};

Ceramic.closeWindow = function(id) {
  Ceramic.windows[id].close()
  Ceramic.windows[id] = null;
};

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
  Ceramic.startWebSockets(process.argv[2],
                          parseInt(process.argv[3]));
});
