import { Elm } from './src/Main.elm'
import { EXIF } from './exif.js'

var app = Elm.Main.init({
  node: document.getElementById('app')
});

app.ports.orientationRequest.subscribe(function(data) {
	var binary = data.split(",")[1];
	var exif = EXIF.readFromBinaryFile(base64ToArrayBuffer(binary));
	var orientation = exif.Orientation

	resetOrientation(data, orientation, function(resetBase64Image) {
		app.ports.orientationResponse.send(
			resetBase64Image
		);
	});
});

function base64ToArrayBuffer (base64) {
    base64 = base64.replace(/^data\:([^\;]+)\;base64,/gmi, '');
    var binaryString = atob(base64);
    var len = binaryString.length;
    var bytes = new Uint8Array(len);
    for (var i = 0; i < len; i++) {
        bytes[i] = binaryString.charCodeAt(i);
    }
    return bytes.buffer;
}

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

function resetOrientation(srcBase64, srcOrientation, callback) {
  var img = new Image();

  img.onload = function() {
    var width = img.width,
        height = img.height,
        canvas = document.createElement('canvas'),
        ctx = canvas.getContext("2d");

    // set proper canvas dimensions before transform & export
		if(img.width < 2000 || img.height < 2000) {
			if (4 < srcOrientation && srcOrientation < 9) {
				canvas.width = height;
				canvas.height = width;
			} else {
				canvas.width = width;
				canvas.height = height;
			}
		} else {
			if (4 < srcOrientation && srcOrientation < 9) {
				canvas.width = height/2;
				canvas.height = width/2;
			} else {
				canvas.width = width/2;
				canvas.height = height/2;
			}
		}

    // transform context before drawing image
		if(img.width < 2000 || img.height < 2000) {
			switch (srcOrientation) {
				case 1: ctx.transform(1, 0, 0, 1, 0, 0); break; // 0
				case 3: ctx.transform(-1, 0, 0, -1, width, height); break; // 180
				case 6: ctx.transform(0, 1, -1, 0, height, 0); break;      // 90
				case 8: ctx.transform(0, -1, 1, 0, 0, width); break;        // -90
				default: break;
			}
		} else {
			switch (srcOrientation) {
				case 1: ctx.transform(0.5, 0, 0, 0.5, 0, 0); break; // 0
				case 3: ctx.transform(-0.5, 0, 0, -0.5, width/2, height/2); break; // 180
				case 6: ctx.transform(0, 0.5, -0.5, 0, height/2, 0); break;      // 90
				case 8: ctx.transform(0, -0.5, 0.5, 0, 0, width/2); break;        // -90
				default: break;
			}
		}

    // draw image
    ctx.drawImage(img, 0, 0);

    // export base64
    callback(canvas.toDataURL('image/jpeg'));
  };

  img.src = srcBase64;
}
