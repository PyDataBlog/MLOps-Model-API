module.exports = function(dataUri, maxDimension, callback){
    var source = new Image();

    source.addEventListener('load', function(){
        var canvas = document.createElement('canvas'),
            ratio = Math.max(source.width, source.height) / maxDimension;

        canvas.width = source.width / ratio;
        canvas.height = source.height / ratio;

        var context = canvas.getContext('2d');

        context.drawImage(
            source,
            0,
            0,
            source.width,
            source.height,
            0,
            0,
            canvas.width,
            canvas.height
        );

        callback(null, canvas.toDataURL());
    });

    source.src = dataUri;
};