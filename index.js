import { Elm } from './src/Main.elm'

var app = Elm.Main.init({
  node: document.getElementById('app')
});

app.ports.cropperData.subscribe(function(data) {
  var url = data.url;
  var size = data.size;
  var resized = data.resized;
  var origin = data.origin;
  var crop = data.crop;

  var canvas = document.createElement('canvas');
  canvas.width = crop.width;
  canvas.height = crop.height;
  var context = canvas.getContext('2d');
  var imageObj = new Image();
  imageObj.src = url;
  imageObj.onload = function() {
    context.save();
    context.beginPath();
    context.arc(250,250, 250, 0,Math.PI*2, true);
    context.closePath();
    context.clip();
    context.drawImage(
        imageObj,
        0,
        0,
        size.width,
        size.height,
        -origin.x,
        -origin.y,
        resized.width,
        resized.height
    );
    context.beginPath();
    context.arc(250,250, 250, 0,Math.PI*2, true);
    context.clip();
    context.closePath();
    context.restore();

    app.ports.testtest.send(
      canvas.toDataURL("image/png", 1.0)
    );
  };
});
