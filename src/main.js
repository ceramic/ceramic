/* JavaScript code for the main process */

var app = require('app');
var readline = require('readline');
var BrowserWindow = require('browser-window');

require('crash-reporter').start();

/* Commands */

// Windows

var window_db = {};

//// Management

function windowCreate(name, options) {
  window_db[name] = new BrowserWindow(options);
  window_db[name].on('closed', function() {
    window_db[name] = null;
  });
};

function windowClose(name) {
  window_db[name].close();
};

function windowDestroy(name) {
  window_db[name].destroy();
};

//// Display

function windowShow(name) {
  window_db[name].show();
};

function windowHide(name) {
  window_db[name].show();
};

function windowResize(name, width_str, height_str) {
  const width = parseInt(width_str);
  const height = parseInt(height_str);
  window_db[name].setSize(width, height)
};

function windowFocus(name) {
  window_db[name].focus();
};

function windowMaximize(name) {
  window_db[name].maximize();
};

function windowUnmaximize(name) {
  window_db[name].unmaximize();
};

function windowMinimize(name) {
  window_db[name].minimize();
};

function windowUnminimize(name) {
  window_db[name].restore();
};

function windowSetFullScreen(name) {
  window_db[name].setFullScreen(true);
};

function windowSetNoFullScreen(name) {
  window_db[name].setFullScreen(false);
};

function windowSetResizable(name) {
  window_db[name].setResizable(true);
};

function windowSetUnResizable(name) {
  window_db[name].setResizable(false);
};

function windowCenter(name) {
  window_db[name].center();
};

function windowSetPosition(name, x_str, y_str) {
  const x = parseInt(x_str);
  const y = parseInt(y_str);
  window_db[name].setPosition(x,y);
};

function windowSetTitle(name, tite) {
  window_db[name].setTitle(title);
};

//// Contents

function windowLoadUrl(name, url) {
  window_db[name].loadUrl(url);
};

function windowReload(name) {
  window_db[name].webContents.reload();
};

function windowOpenDevTools(name) {
  window_db[name].openDevTools();
};

function windowCloseDevTools(name) {
  window_db[name].closeDevTools();
}

////// Text

function windowUndo(name) {
  window_db[name].webContents.undo();
};

function windowRedo(name) {
  window_db[name].webContents.redo();
};

function windowCut(name) {
  window_db[name].webContents.cut();
};

function windowCopy(name) {
  window_db[name].webContents.copy();
};

function windowPaste(name) {
  window_db[name].webContents.paste();
};

function windowDelete(name) {
  window_db[name].webContents.delete();
};

function windowSelectAll(name) {
  window_db[name].webContents.selectAll();
};

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
    windowCreate(data['name'], data);
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
