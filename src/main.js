/* JavaScript code for the main process */

var app = require('app');
var readline = require('readline');
var BrowserWindow = require('browser-window');

require('crash-reporter').start();

/* Window management */

var window_db = {};

function createWindow(name, options) {
  window_db[name] = new BrowserWindow(options);
  window_db[name].on('closed', function() {
    window_db[name] = null;
  });
};

function windowLoadUrl(name, url) {
  window_db[name].loadUrl(url);
};

function windowOpenDevTools(name, url) {
  window_db[name].openDevTools(url);
};

function windowShow(name) {
  window_db[name].show();
};

function windowResize(name, width, height) {
  window_db[name].setSize(width, height)
}

/* Lifecycle management */

function quit() {
  app.quit();
};

app.on('window-all-closed', function() {
  if (process.platform != 'darwin') {
    quit();
  }
});

/* Command dispatcher */

var iointerface = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

const dispatcher = {
  'create-window': function(data) {
    createWindow(data['name'], data);
  },
  'load-url': function(data) {
    windowLoadUrl(data['name'], data['url']);
  },
  'show-window': function(data) {
    windowShow(data['name']);
  },
  'quit': function(data) {
    quit();
  }
};

function dispatchCommand(data) {
  const command = data['cmd'];
  dispatcher[command](data);
};

/* Start up */

app.on('ready', function() {
  /* Start listening for commands on the server */
  iointerface.on('line', function (line) {
    dispatchCommand(JSON.parse(line));
  });
});
