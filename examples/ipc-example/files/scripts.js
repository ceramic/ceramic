var ipc = require('ipc');

document.addEventListener("DOMContentLoaded", function(event) {
  var node = document.getElementById('button');

  node.onclick = function() {
    ipc.send('ceramic-channel', { text: 'Clicked button' });
  };
});
